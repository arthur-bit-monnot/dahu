package dahu.model.problem

import cats._
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.model.ir._
import dahu.model.problem.ContextualLazyForestMap.ContextualPreprocessor
import dahu.utils._
import spire.sp

import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag

object ExpandLambdas {

  case class LambdaDeps[@sp(Int) I](params: Set[I],
                                    applicationStack: List[I],
                                    forApply: Option[I] = None,
                                    static: Boolean = false) {

    def combine(o: LambdaDeps[I]): LambdaDeps[I] = {
      assert(applicationStack.isEmpty && o.applicationStack.isEmpty)
      if(params.isEmpty) o
      else if(o.params.isEmpty) this
      else LambdaDeps(params ++ o.params, Nil, None, static && o.static)
    }

    val subs: Bag[I] =
      Bag.fromIterables2(params, applicationStack, forApply.toList)

    def map[@sp(Int) B](f: I => B): LambdaDeps[B] =
      LambdaDeps(params.map(f), applicationStack.map(f), forApply.map(f), static)
  }

  object LambdaDeps {
    private val singleton = LambdaDeps[Int](Set(), List(), None, true)
    def empty[I <: Int]: LambdaDeps[I] = singleton.asInstanceOf[LambdaDeps[I]]
  }

  case class N[@sp(Int) X](e: StaticF[X], deps: LambdaDeps[X])
  implicit val treeNode: TreeNode[N] = new TreeNode[N] {
    override def children[K](n: N[K]): immutable.Iterable[K] =
      Bag.Cons(TreeNode[StaticF].children(n.e), n.deps.subs)

    override def foreachChild[K](n: N[K])(f: K => Unit): Unit = {
      n.e.foreachChild(f)
      n.deps.subs.foreach(f)
    }
  }
  implicit val sfunctorInstance: SFunctor[N] = new SFunctor[N] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: N[A])(f: A => B): N[B] =
      N(fa.e.smap(f), fa.deps.map(f))
  }

  def expandLambdas[X](lazyTree: RootedASG[X, StaticF, cats.Id]): RootedASG[X, Total, cats.Id] = {
    val tree = lazyTree.fixID.forceEvaluation

    val x = tree.tree.cataLow(lambdaDependencies)
    type I = tree.ID

    val XX: I => N[I] = i => N(tree.tree.internalCoalgebra(i), x.getInternal(i))

    val t2 =
      new ContextualLazyForestMap2[I, N, Total, IDTop](XX, compiler2, new MyPrepro2(Map()))
    val opt = tree.tree.getTreeRoot(tree.root)

    val t3 = t2.changedKey[X](x => tree.tree.getTreeRoot(x))
    t3.rootedAt(tree.root)
  }

  def lambdaDependencies[I <: IDTop]: (I, StaticF[LambdaDeps[I]]) => LambdaDeps[I] = (a, b) => {
    (a, b) match {
      case (i, _: LambdaParamF[_]) => LambdaDeps(Set(i), Nil, None, false)
      case (i, LambdaF(in, tree, _, _)) =>
        assert(in.params.size == 1 && in.applicationStack.isEmpty)
        assert(tree.params.contains(in.params.head))
        LambdaDeps(tree.params ++ in.params, in.params.head :: tree.applicationStack, None, false)
      case (i, ApplyF(lbd, param, _)) =>
        assert(param.applicationStack.isEmpty)
        assert(lbd.applicationStack.nonEmpty)
        LambdaDeps(
          lbd.params ++ param.params - lbd.applicationStack.head,
          lbd.applicationStack.tail,
          Some(lbd.applicationStack.head),
          false
        )
      case (_, x: StaticF[LambdaDeps[I]]) =>
        TreeNode[StaticF]
          .childrenFoldLeft(x)(LambdaDeps.empty[I])((a, b) => a.combine(b))
    }
  }

  case class MyPrepro2[I <: IDTop](map: Map[I, I] = Map[I, I]())
      extends ContextualPreprocessor[N, I] {

    override val hashCode: Int = map.hashCode()

    override def prepro(i: I, coalg: I => N[I]): (I, MyPrepro2[I]) = {
      val i2 = map.getOrElse(i, i)
      val tmp = subPreprocessor(coalg, i2)
      (tmp.map.getOrElse(i2, i2), tmp)
    }

    def subPreprocessor(coalg: I => N[I], i: I): MyPrepro2[I] = {
      val fi = coalg(i)
      val res = fi match {
        case x @ N(ApplyF(lbd, param, _), LambdaDeps(_, _, Some(cur), _)) =>
//          def getLambdaParam(i: I, nesting: Int): (I, I) = coalg(i) match {
//            case N(LambdaF(arg, tree, _, _), _) if nesting == 0 => (arg, tree)
//            case N(LambdaF(_, tree, _, _), _)                   => getLambdaParam(tree, nesting - 1)
//            case N(ApplyF(x, _, _), _)                          => getLambdaParam(x, nesting + 1)
//            case N(x, _)                                        => dahu.utils.errors.unexpected(x.toString)
//          }
//
//          val (lambdaArg, lambdaTree) = getLambdaParam(lbd, nesting = 0)
//          assert(cur == lambdaArg)
//          val XXX = coalg(param)
          MyPrepro2(map.updated(cur, param))
        case x @ N(LambdaParamF(_, _), deps) if !deps.params.contains(i) =>
          dahu.utils.errors.unexpected

        case x @ N(_: ApplyF[_], _) => dahu.utils.errors.unexpected
        case N(a, deps) =>
          if(this.map.isEmpty)
            this
          else if(map.keys.forall(x => !deps.params.contains(x)))
            MyPrepro2.empty[I]
          else if(map.keys.exists(x => !deps.params.contains(x)))
            MyPrepro2(map.filterKeys(k => deps.params.contains(k)))
          else
            this
      }
      res
    }
  }
  object MyPrepro2 {
    private val emptySingleton = new MyPrepro2[IDTop]()
    def empty[I <: IDTop]: MyPrepro2[I] = emptySingleton.asInstanceOf[MyPrepro2[I]]
  }

  def compiler2[I <: IDTop](genCtx: Context[Total, I]): N[cats.Id[I]] => Total[cats.Id[I]] = {
    case x @ N(ApplyF(lambda, param, typ), _) =>
      // recurse on the tree of lambda, replacing, lbd.in by param
      genCtx.retrieve(lambda) match {
        case LambdaF(_, tree, _, _) => genCtx.retrieve(tree)
        case _                      => dahu.utils.errors.unexpected
      }
    case N(x: Total[Id[I]], _) => x
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
      extends OpenASG[OID, FOut, cats.Id, InternalID] {

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

//    private val preproUsage = mutable.Map[(OID, PrePro), Set[(OID, OID)]]()
//    private val uselessBinds = mutable.Map[OID, Set[OID]]()

    override def getTreeRoot(oid: OID): ID = {
      val queue = mutable.ArrayStack[(OID, PrePro)]()
      val init = contextualPreprocessor.prepro(oid, coalg)
      queue.push(init)
      perRequest = 0

      while(queue.nonEmpty) {
        val next = queue.pop()
        if(!intIdsMap.contains(next)) {
          counter += 1
          perRequest += 1
//          if(counter % 10000 == 0)
//            println(s"$counter \t $cacheHit / $cacheMiss -- ${intIdsMap.size}")
          val (cur, ctx) = next
          val fkNoPrepro = coalg(cur)
          val fk = fkNoPrepro.smap(ctx.prepro(_, coalg))

//          val directPreprocessing = fkNoPrepro.children
//            .map(i => (i, subCtx.prepro(i, coalg)))
//            .filter(x => x._1 != x._2)
//            .toSet
////          println("processing: " + next)
//
//          def addPreprocessingData(): Unit = {
//            val usage = (fk.children.toSet
//              .filter(_ != cur)
//              .flatMap((i: OID) => preproUsage((i, subCtx)))
//              ++ directPreprocessing)
//              .filter(x => ctx.map.keySet.contains(x._1))
////
//            preproUsage += (((cur, ctx), usage))
//            val used = usage.map(_._1)
//            val all = ctx.map.keySet
//            val unused = all -- used
//            uselessBinds(cur) = uselessBinds.getOrElse(cur, Set()) ++ unused
//            if(all.nonEmpty)
//              println(
//                s"${used.size.toDouble / all.size}  ${used.size} / ${all.size}    $used    //  $all")
//          }

          if(fk.forallChildren(c => (c == next) || intIdsMap.contains(c))) {
            if(fk.childrenContains(next)) {
              val FUTURE_ID: ID = repMap.length.asInstanceOf[ID]
              val fing = fk.smap(c => if(c == next) FUTURE_ID else intIdsMap(c))
              val foutg = algebra(fing)
              dahu.utils.debug.assert4(!foutg.children.toSet.contains(FUTURE_ID))
              val g: ID = record(foutg)
              intIdsMap += ((next, g))
//              addPreprocessingData()
//              println(s"$cur  $ctx  $g")
            } else {
              val fing = fk.smap(c => intIdsMap(c))
              val foutg = algebra(fing)
              val g: ID = record(foutg)
              intIdsMap += ((next, g))
//              addPreprocessingData()
//              println(s"$cur  $ctx  $g")
            }
          } else {
            queue.push((cur, ctx))
            fk.foreachChild(c => {
              if(c != next)
                queue.push(c)
            })
          }
        }
      }

//      println {
//        val x = intIdsMap.keysSet
//          .toScalaSet()
//          .groupBy(_._1)
//          .mapValues(z => (z.size, z.map(_._2)))
//          .toSeq
//          .map { case (i, (n, ctxs)) => (n, i, coalg(i), ctxs) }
//          .sortBy(_._1)
//          .reverse
//          .take(1000)
//        x.foreach {
//          case (n, i, orig, ctxs) => println(s"$n  ($i) \t\t     $orig                   $ctxs")
//        }
//        println("a")
//      }
      intIdsMap(init)
    }
  }

}

object ContextualLazyForestMap {
  abstract class ContextualPreprocessor[F[_], I <: Int] {
//    def subPreprocessor(coalg: I => F[I], i: I): ContextualPreprocessor[F, I]
    def prepro(i: I, coalg: I => F[I]): (I, ContextualPreprocessor[F, I])
  }
}
