package dahu.model.problem

import cats._
import cats.implicits._
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.model.compiler.Algebras
import dahu.model.ir._
import dahu.model.problem.ContextualLazyForestMap.ContextualPreprocessor
import dahu.model.problem.ExpandLambdas.LambdaDeps
import dahu.recursion.Recursion
import dahu.utils.SFunctor
import dahu.utils.SFunctor._
import dahu.utils.debug._
import spire.sp

import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag

object ExpandLambdas {

//  case class LambdaTok[@sp(Int) I](lbd: I, param: I) {
//    def subs: immutable.Iterable[I] = immutable.Iterable(lbd, param)
//    def map[@sp(Int) B](f: I => B): LambdaTok[B] = LambdaTok(f(lbd), f(param))
//  }
  case class LambdaDeps[@sp(Int) I](params: Set[I],
                                    treesBelow: Set[I],
                                    applicationStack: List[I],
                                    forApply: Option[I],
                                    static: Boolean) {

    def combine(o: LambdaDeps[I]): LambdaDeps[I] = {
      assert(applicationStack.isEmpty && o.applicationStack.isEmpty)
      if(params.isEmpty && treesBelow.isEmpty) o
      else if(o.params.isEmpty && treesBelow.isEmpty) this
      else LambdaDeps(params ++ o.params, treesBelow ++ treesBelow, Nil, None, static && o.static)
    }

    def subs: immutable.Iterable[I] =
      params ++ treesBelow ++ applicationStack ++ forApply.toIterable
    def map[@sp(Int) B](f: I => B): LambdaDeps[B] =
      LambdaDeps(params.map(f), treesBelow.map(f), applicationStack.map(f), forApply.map(f), static)

    val deps = params ++ forApply
  }

  case class N[@sp(Int) X](e: StaticF[X], deps: LambdaDeps[X])
  implicit val treeNode: TreeNode[N] = new TreeNode[N] {
    override def children[K](n: N[K]): immutable.Iterable[K] =
      TreeNode[StaticF].children(n.e) ++ n.deps.subs
  }
  implicit val sfunctorInstance: SFunctor[N] = new SFunctor[N] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: N[A])(f: A => B): N[B] =
      N(fa.e.smap(f), fa.deps.map(f))
  }

  def expandLambdas[X](
      lazyTree: LazyTree[X, StaticF, cats.Id, _]): LazyTree[X, NoApplyF, cats.Id, _] = {
    info("entering expand lamdas")
    val tree = lazyTree.fixID.forceEvaluation
    info("defining forest")

    val x = tree.tree.cataLow(lambdaDependencies)
    //x.get(tree.root)
    type I = tree.ID

    val XX: I => N[I] = i => N(tree.tree.internalCoalgebra(i), x.getInternal(i))

//    println(Recursion.ana(XX)(tree.tree.getTreeRoot(tree.root)))
//    sys.exit()

    val t2 =
      new ContextualLazyForestMap2[I, N, NoApplyF, IDTop](XX, compiler2, new MyPrepro2(Map(), None))
    val opt = tree.tree.getTreeRoot(tree.root)
//    println(opt.map(t2.getTreeRoot))
//    println("dd")
    val t3 = t2.changedKey[X](x => tree.tree.getTreeRoot(x))
    LazyTree(t3)(tree.root)
  }

  def lambdaDependencies[I <: IDTop]: (I, StaticF[LambdaDeps[I]]) => LambdaDeps[I] = (a, b) => {
    (a, b) match {
      case (i, _: LambdaParamF[_]) => LambdaDeps(Set(i), Set(), Nil, None, false)
      case (i, LambdaF(in, tree, _, _)) =>
        assert(in.params.size == 1 && in.applicationStack.isEmpty)
        assert(tree.params.contains(in.params.head))
        LambdaDeps(tree.deps ++ in.deps,
                   tree.treesBelow + i,
                   in.params.head :: tree.applicationStack,
                   None,
                   false)
      case (i, ApplyF(lbd, param, _)) =>
        assert(param.applicationStack.isEmpty)
        assert(lbd.applicationStack.nonEmpty)
        LambdaDeps(
          lbd.params ++ param.params - lbd.applicationStack.head,
          lbd.treesBelow ++ param.treesBelow,
          lbd.applicationStack.tail,
          Some(lbd.applicationStack.head),
          false
        )
      case (_, x: StaticF[LambdaDeps[I]]) =>
        TreeNode[StaticF]
          .children(x)
          .foldLeft(LambdaDeps[I](Set(), Set(), Nil, None, true))((a, b) => a.combine(b))
    }
  }

  class MyPrepro2[I <: IDTop](val map: Map[I, (I, LambdaDeps[I], I)] = Map(),
                              val parent: Option[(N[I], MyPrepro2[I])])
      extends ContextualPreprocessor[N, I] {

    val hist: List[MyPrepro2[I]] = parent.map(p => p._2 :: p._2.hist).getOrElse(Nil)

    override def hashCode(): Int = map.hashCode()

    override def equals(o: scala.Any): Boolean = o match {
      case x: MyPrepro2[_] => map == x.map
    }

    override def toString: String = map.toString()

    override def prepro(i: I, coalg: I => N[I]): I = {
//      coalg(i) match {
//        case N(LambdaParamF(_, _), _) =>
//          val fi = coalg(i)
//          assert(map.contains(i), fi.toString)
//        case _ =>
//          assert(!map.contains(i))
//      }
      map.get(i).map(_._1).getOrElse(i)
    }

    override def subPreprocessor(coalg: I => N[I], i: I): ContextualPreprocessor[N, I] = {
      val fi = coalg(i)
//      if(i == 90) {
//        println("STOP")
//      }
      val res = fi match {
        case x @ N(ApplyF(lbd, param, _), LambdaDeps(_, _, _, Some(cur), _)) =>
          def getLambdaParam(i: I, nesting: Int): (I, I) = coalg(i) match {
            case N(LambdaF(arg, tree, _, _), _) if nesting == 0 => (arg, tree)
            case N(LambdaF(_, tree, _, _), _)                   => getLambdaParam(tree, nesting - 1)
            case N(ApplyF(x, _, _), _)                          => getLambdaParam(x, nesting + 1)
            case N(x, _)                                        => dahu.utils.errors.unexpected(x.toString)
          }

          val (lambdaArg, lambdaTree) = getLambdaParam(lbd, nesting = 0)
          assert(cur == lambdaArg)
          val XXX = coalg(param)
          new MyPrepro2(map + (lambdaArg -> ((param, XXX.deps, lambdaTree))), Some((fi, this)))
        case x @ N(LambdaParamF(_, _), deps) if !deps.params.contains(i) =>
          dahu.utils.errors.unexpected

        case x @ N(_: ApplyF[_], _) => dahu.utils.errors.unexpected
        case N(_, deps)             =>
//          if(map.keys.exists(i => !deps.params.contains(i)))
//            println("BREAK")

//          val indirectTrees = deps.treesBelow ++ map.values.toSet
//            .flatMap((x: (I, LambdaDeps[I], I)) => x._2.treesBelow)

//          val withIndirectDeps: Set[I] = deps.deps ++
//            map.values.toSet
//              .filter(_._1 != i) // if we are the result of a transformation, ignore it
//              .filter {
//                case (_, _, targetTree) => {
//                  if(!indirectTrees.contains(targetTree))
//                    println("dddd")
//                }
//                true
//              }
//              .flatMap((x: (I, LambdaDeps[I], I)) => x._2.deps)
//          val withIndirectDeps2 =
//            withIndirectDeps
//              .filterNot(dep => map.get(dep).map(_._1).contains(i))

//          if(i == 90)
//            println("STOP")
//          if(deps.static) {
//            val TMP = map.filterKeys(k => withIndirectDeps2.contains(k))
//            if(TMP.nonEmpty) {
//              println("BREAK" + TMP)
//            }
//            new MyPrepro2[I](Map(), None)
//          } else {
          new MyPrepro2(map.filterKeys(k => deps.deps.contains(k)), Some((fi, this)))
//          }
//          this
        //          println(s"${deps.size} / ${map.size}         ${map.keys}  /  ${deps}")

      }
      if(this.map.nonEmpty && fi.e.isInstanceOf[InputF[_]]) {
        val x = res.hist.flatMap(_.parent.map(x => s"${x._1}     -------    ${x._2.map}").toList)
        println(s"STOP + $x")
      }
//      println(s"$i ${fi.e} \n  ${fi.deps}\n  $this\n  $res")
//      if(this.map.keySet.contains(72.asInstanceOf[I]) && !res.map.keySet.contains(
//           72.asInstanceOf[I]))
//        println("break")
      res
    }
  }

  def compiler2[I <: IDTop](genCtx: Context[NoApplyF, I]): N[cats.Id[I]] => NoApplyF[cats.Id[I]] = {
    case x @ N(ApplyF(lambda, param, typ), _) =>
      // recurse on the tree of lambda, replacing, lbd.in by param
      genCtx.retrieve(lambda) match {
        case LambdaF(_, tree, _, _) => genCtx.retrieve(tree)
        case _                      => dahu.utils.errors.unexpected
      }
    case N(x: NoApplyF[Id[I]], _) => x
  }

  final class Context[M[_], A <: Int](val ret: A => M[A]) {
    def retrieve(a: A): M[A] = ret(a)
  }

  class ContextualLazyForestMap2[OID <: IDTop,
                                 FIn[_]: TreeNode: SFunctor,
                                 FOut[_]: TreeNode,
                                 InternalID <: IDTop](
      coalg: OID => FIn[OID],
      compilerGenerator: Context[FOut, InternalID] => FIn[InternalID] => FOut[InternalID],
      contextualPreprocessor: ContextualLazyForestMap.ContextualPreprocessor[FIn, OID])(
      implicit ct: ClassTag[FOut[InternalID]])
      extends IlazyForest[OID, FOut, cats.Id, InternalID] {

    private type PrePro = ContextualLazyForestMap.ContextualPreprocessor[FIn, OID]

    private val intIdsMap = debox.Map[(OID, PrePro), ID]()
    private val repMap = debox.Buffer[FOut[ID]]() // ID => FOut[ID]
    private val memo = debox.Map[FOut[ID], ID]()

    private var cacheHit = 0
    private var cacheMiss = 0

    private def record(e: FOut[ID]): ID = {
      if(memo.contains(e)) {
        cacheHit += 1
        memo(e)
      } else {
        cacheMiss += 1
        val id = repMap.length.asInstanceOf[ID]
        repMap += e
        memo += ((e, id))
        id
      }
    }
    private val generationContext: Context[FOut, ID] = new Context(internalCoalgebra)

    val algebra: FIn[ID] => FOut[ID] = compilerGenerator(generationContext)

    override def internalCoalgebra(i: ID): FOut[ID] = repMap(i)

    private var perRequest = 0
    private var counter = 0

    private val preproUsage = mutable.Map[(OID, PrePro), Set[(OID, OID)]]()
    private val uselessBinds = mutable.Map[OID, Set[OID]]()

    override def getTreeRoot(oid: OID): ID = {
      val queue = mutable.ArrayStack[(OID, PrePro)]()
      queue.push((oid, contextualPreprocessor))
      perRequest = 0

      while(queue.nonEmpty) {
        val next = queue.pop()
        if(!intIdsMap.contains(next)) {
//          println(next)
          counter += 1
          perRequest += 1
          if(counter % 10000 == 0)
            println(s"$counter \t $cacheHit / $cacheMiss -- ${intIdsMap.size}")
          val (cur, ctx) = next
          val fkNoPrepro = coalg(cur) //originalCoalgebra(cur)
          val subCtx = ctx.subPreprocessor(coalg, cur)
          val fk = fkNoPrepro.smap(subCtx.prepro(_, coalg))

//          val directPreprocessing = fkNoPrepro.children
//            .map(i => (i, subCtx.prepro(i, coalg)))
//            .filter(x => x._1 != x._2)
//            .toSet
//          println("processing: " + next)

//          def addPreprocessingData(): Unit = {
//            val usage = (fk.children.toSet
//              .filter(_ != cur)
//              .flatMap((i: OID) => preproUsage((i, subCtx)))
//              ++ directPreprocessing)
//              .filter(x => ctx.map.keySet.contains(x._1))
//
//            preproUsage += (((cur, ctx), usage))
//            val used = usage.map(_._1)
//            val all = ctx.map.keySet
//            val unused = all -- used
//            uselessBinds(cur) = uselessBinds.getOrElse(cur, Set()) ++ unused
//            if(all.nonEmpty)
//              println(s"${used.size} / ${all.size}    $used    //  $all")
//          }

          if(fk.children.forall(c => (c == cur) || intIdsMap.contains((c, subCtx)))) {
            if(fk.children.toSet.contains(cur)) {
              val FUTURE_ID: ID = repMap.length.asInstanceOf[ID]
              val fing = fk.smap(c => if(c == cur) FUTURE_ID else intIdsMap((c, subCtx)))
              val foutg = algebra(fing)
              assert(!foutg.children.toSet.contains(FUTURE_ID))
              val g: ID = record(foutg)
              intIdsMap += (((cur, ctx), g))
//              addPreprocessingData()
//              println(s"$cur  $ctx  $g")
            } else {
              val fing = fk.smap(c => intIdsMap((c, subCtx)))
              val foutg = algebra(fing)
              val g: ID = record(foutg)
              intIdsMap += (((cur, ctx), g))
//              addPreprocessingData()
//              println(s"$cur  $ctx  $g")
            }
          } else {
            queue.push((cur, ctx))

            fk.children.foreach(c => {
              if(c != cur)
                queue.push((c, subCtx))
            })
          }
        }
      }

//      val num = intIdsMap.keysSet.toScalaSet().groupBy(_._1).values.size
//      val totalKeys = intIdsMap.keysSet.toScalaSet().groupBy(_._1).values.map(_.size).sum
//      println(s"$num / $totalKeys = ${totalKeys / num}")

      intIdsMap((oid, contextualPreprocessor))
    }
  }

}

//class ContextualLazyForestMap[
//    K,
//    FIn[_]: TreeNode: SFunctor,
//    FOut[_],
//    Opt[_]: Functor,
//    InternalID <: IDTop,
//    OID <: IDTop](base: IlazyForest[K, FIn, Opt, OID],
//                  compilerGenerator: LazyForestGenerator.Context[FOut, InternalID] => FIn[
//                    InternalID] => InternalID,
//                  contextualPreprocessor: ContextualLazyForestMap.ContextualPreprocessor[FIn, OID])(
//    implicit ct: ClassTag[FOut[InternalID]])
//    extends IlazyForest[K, FOut, Opt, InternalID] {
//
//  private type PrePro = ContextualLazyForestMap.ContextualPreprocessor[FIn, OID]
//
//  private val intIdsMap = debox.Map[(OID, PrePro), ID]()
//  private val repMap = debox.Buffer[FOut[ID]]() // ID => FOut[ID]
//  private val memo = debox.Map[FOut[ID], ID]()
//
//  private var cacheHit = 0
//  private var cacheMiss = 0
//
//  private def record(e: FOut[ID]): ID = {
//    if(memo.contains(e)) {
//      cacheHit += 1
//      memo(e)
//    } else {
//      cacheMiss += 1
//      val id = repMap.length.asInstanceOf[ID]
//      repMap += e
//      memo += ((e, id))
//      id
//    }
//  }
//  private val generationContext: LazyForestGenerator.Context[FOut, ID] =
//    new LazyForestGenerator.Context(record, internalCoalgebra)
//
//  def algebra: FIn[ID] => ID = compilerGenerator(generationContext)
//
//  override def internalCoalgebra(i: ID): FOut[ID] = repMap(i)
//
//  private var perRequest = 0
//  private var counter = 0
//
//  def getFromOriginalId(oid: base.ID): ID = {
//    val queue = mutable.Stack[(OID, PrePro)]()
//    queue.push((oid, contextualPreprocessor))
//    perRequest = 0
//
//    while(queue.nonEmpty) {
//      val next = queue.pop()
//      if(!intIdsMap.contains(next)) {
//        println(next)
//        counter += 1
//        perRequest += 1
//        //        if(counter % 10 == 0)
//        //          println(s"$perRequest \t $counter \t $cacheHit / $cacheMiss -- ${intIdsMap.size}")
//        val (cur, ctx) = next
//        val fkNoPrepro = base.internalCoalgebra(cur) //originalCoalgebra(cur)
//        val subCtx = ctx.subPreprocessor(base.internalCoalgebra, fkNoPrepro)
//        val fk = fkNoPrepro.smap(subCtx.prepro)
//        if(fk.children.forall(c => intIdsMap.contains((c, subCtx)))) {
//          val fg = fk.smap(c => intIdsMap((c, subCtx)))
//          val g: ID = algebra(fg)
//          intIdsMap += (((cur, ctx), g))
//          println(s"$next -> $g")
//        } else {
//          queue.push((cur, ctx))
//
//          fk.children.foreach(c => queue.push((c, subCtx)))
//        }
//      }
//    }
//
//    val num = intIdsMap.keysSet.toScalaSet().groupBy(_._1).values.size
//    val totalKeys = intIdsMap.keysSet.toScalaSet().groupBy(_._1).values.map(_.size).sum
//    println(s"$num / $totalKeys = ${totalKeys / num}")
//    intIdsMap((oid, contextualPreprocessor))
//  }
//
//  override def getTreeRoot(key: K): Opt[ID] = {
//    base.getTreeRoot(key).map(getFromOriginalId)
//  }
//}

object ContextualLazyForestMap {
  abstract class ContextualPreprocessor[F[_], I <: Int] {
    def map: Map[I, (I, LambdaDeps[I], I)]
    def subPreprocessor(coalg: I => F[I], i: I): ContextualPreprocessor[F, I]
    def prepro(i: I, coalg: I => F[I]): I
  }
}
