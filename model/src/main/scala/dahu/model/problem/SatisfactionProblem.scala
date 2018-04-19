package dahu.model.problem

import cats.Functor
import cats.implicits._
import dahu.maps.{ArrayMap, Counter, Wrapped}
import dahu.maps.growable.GrowableBiMap
import dahu.model.compiler.Algebras
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.problem.SatisfactionProblemFAST.{ILazyTree, RootedLazyTree}
import dahu.model.types._
import dahu.recursion._

import scala.collection.mutable

object SatisfactionProblem {

  def satisfactionSubAST(ast: AST[_]): RootedLazyTree[ast.ID, Total, cats.Id] = {
    SatisfactionProblemFAST.encode(ast.root, ast.tree.asFunction)
//    val start = System.currentTimeMillis()
//    val conditionFast = SatisfactionProblemFAST.encode(ast.root, ast.tree.asFunction)
//    val inter = System.currentTimeMillis()
////    val conditionSlow = encode(ast.root, ast.tree.asFunction)
//    val end = System.currentTimeMillis()
//    println(s"opt:  ${inter - start}")
//    println(s"slow: ${end - inter}")
//
//    val condition = conditionFast
//
//    val memory = mutable.LinkedHashMap[Total[Int], Int]()
//
//    val alg: Total[Int] => Int = env => {
//      memory.getOrElseUpdate(env, memory.size)
//    }
//    val treeRoot = Recursion.cata[Total, Int](alg)(condition)
//    val reversedMemory = memory.map(_.swap).toMap
//    val genTree = ArrayMap.build(reversedMemory.keys, k => reversedMemory(k))
//
//    new TotalSubAST[ast.ID] {
//      override def tree: ArrayMap.Aux[ID, Total[ID]] =
//        genTree.asInstanceOf[ArrayMap.Aux[ID, Total[ID]]]
//
//      override def root: ID = treeRoot.asInstanceOf[ID]
//
//      override val subset: TotalSubAST.SubSet[ast.ID, ID] = new TotalSubAST.SubSet[ast.ID, ID] {
//        override def from: ID => Option[ast.ID] = x => {
//          val e = tree(x)
//          ast.reverseTree.get(e.asInstanceOf[ast.Expr])
//        }
//        override def to: ast.ID => Option[ID] = x => {
//          val e: ExprF[ast.ID] = ast.tree(x)
//          e match {
//            case t: Total[_] => reverseTree.get(t.asInstanceOf[Total[ID]])
//            case _           => None
//          }
//        }
//
//      }
//    }
  }

  type PB = Partial[Fix[Total]]

  private object Utils {
    import scala.language.implicitConversions
    implicit def autoFix[F[_]](x: F[Fix[F]]): Fix[F] = Fix(x)

    def and(conjuncts: Fix[Total]*): Fix[Total] = {
      assert(conjuncts.forall(c => c.unfix.typ == Tag.ofBoolean))
      val nonEmptyConjuncts = conjuncts.filter {
        case ComputationF(bool.And, Seq(), _) => false
        case CstF(true, _)                    => false
        case _                                => true
      }
      ComputationF(bool.And, nonEmptyConjuncts, Tag.ofBoolean)
    }
    def not(e: Fix[Total]): Fix[Total] = {
      assert(e.unfix.typ == Tag.ofBoolean)
      ComputationF(bool.Not, Seq(e), Tag.ofBoolean)
    }
    def implies(cond: Fix[Total], eff: Fix[Total]): Fix[Total] = {
      assert(cond.unfix.typ == Tag.ofBoolean && eff.unfix.typ == Tag.ofBoolean)
      val notCond = Fix(ComputationF(bool.Not, Seq(cond), Tag.ofBoolean))
      ComputationF(bool.Or, Seq(notCond, eff), Tag.ofBoolean)
    }
  }
  import Utils._

  var cacheMiss = 0
  var cacheHit = 0

  case class IR(value: Fix[Total], present: Fix[Total], valid: Fix[Total])
  def compiler(cache: mutable.Map[ExprF[IR], IR], optimize: Boolean): FAlgebra[ExprF, IR] = x => {
    val ir = x match {
      case x if cache.contains(x) =>
        cacheHit += 1
        cache(x)
      case x: InputF[_] => IR(Fix(x), and(), and())
      case x: CstF[_]   => IR(Fix(x), and(), and())
      case ComputationF(f, args, t) =>
        IR(
          value = ComputationF(f, args.map(a => a.value), t),
          present = and(args.map(_.present): _*),
          valid = and(args.map(_.valid): _*)
        )
      case ProductF(members, t) =>
        IR(
          value = ProductF(members.map(a => a.value), t),
          present = and(members.map(_.present): _*),
          valid = and(members.map(_.valid): _*)
        )
      case ITEF(cond, onTrue, onFalse, t) =>
        IR(
          value = ITEF(cond.value, onTrue.value, onFalse.value, t),
          present = and(cond.present,
                        implies(cond.value, onTrue.present),
                        implies(not(cond.value), onFalse.present)),
          valid = and(cond.valid,
                      implies(cond.value, onTrue.valid),
                      implies(not(cond.value), onFalse.valid))
        )
      case OptionalF(value, present, _) =>
        IR(
          value = value.value,
          present = and(value.present, present.present, present.value),
          valid = and(present.valid, value.valid)
        )
      case PresentF(opt) =>
        IR(
          value = opt.present,
          present = and(),
          valid = opt.valid
        )
      case ValidF(part) =>
        IR(
          value = part.valid,
          present = part.present,
          valid = and()
        )
      case Partial(value, condition, tpe) =>
        IR(
          value = value.value,
          present = value.present,
          valid =
            and(value.valid, implies(condition.present, and(condition.value, condition.valid)))
        )
    }

    if(!cache.contains(x)) {
      cacheMiss += 1
      cache += ((x, ir))
    }
    ir
  }

  def optimizer(tot: Fix[Total]): Fix[Total] =
    dahu.recursion.Recursion
      .cata[Total, Fix[Total]](dahu.model.compiler.Optimizations.simplificationAlgebra)(tot)

  def encode[X](root: X, coalgebra: FCoalgebra[ExprF, X], optimize: Boolean = true): Fix[Total] = {
    val cache = mutable.Map[ExprF[IR], IR]()
    val ir = Recursion.hylo(coalgebra, compiler(cache, optimize = optimize))(root)

    println(s"MISS / HIT : $cacheMiss / $cacheHit")

//    println(Algebras.format(ir.valid))
    if(optimize)
      optimizer(ir.valid)
    else
      ir.valid
  }

}

object SatisfactionProblemFAST {

  def satisfactionSubAST(ast: AST[_]): TotalSubAST[ast.ID] = {
    val condition = encode(ast.root, ast.tree.asFunction)
    ???
//    val memory = mutable.LinkedHashMap[Total[Int], Int]()
//
//    val alg: Total[Int] => Int = env => {
//      memory.getOrElseUpdate(env, memory.size)
//    }
//    val treeRoot = Recursion.cata[Total, Int](alg)(condition)
//    val reversedMemory = memory.map(_.swap).toMap
//    val genTree = ArrayMap.build(reversedMemory.keys, k => reversedMemory(k))
//
//    new TotalSubAST[ast.ID] {
//      override def tree: ArrayMap.Aux[ID, Total[ID]] =
//        genTree.asInstanceOf[ArrayMap.Aux[ID, Total[ID]]]
//
//      override def root: ID = treeRoot.asInstanceOf[ID]
//
//      override val subset: TotalSubAST.SubSet[ast.ID, ID] = new TotalSubAST.SubSet[ast.ID, ID] {
//        override def from: ID => Option[ast.ID] = x => {
//          val e = tree(x)
//          ast.reverseTree.get(e.asInstanceOf[ast.Expr])
//        }
//        override def to: ast.ID => Option[ID] = x => {
//          val e: ExprF[ast.ID] = ast.tree(x)
//          e match {
//            case t: Total[_] => reverseTree.get(t.asInstanceOf[Total[ID]])
//            case _           => None
//          }
//        }
//
//      }
//    }
  }

  type ID = Int

  private object Utils {
    import scala.language.implicitConversions
    implicit def autoFix[F[_]](x: F[Fix[F]]): Fix[F] = Fix(x)

    def and(conjuncts: ID*)(implicit record: Total[ID] => ID): ID = {
      record(ComputationF(bool.And, conjuncts, Tag.ofBoolean))
//      assert(conjuncts.forall(c => c.unfix.typ == Tag.ofBoolean))
//      val nonEmptyConjuncts = conjuncts.filter {
//        case ComputationF(bool.And, Seq(), _) => false
//        case CstF(true, _)                    => false
//        case _                                => true
//      }
//      ComputationF(bool.And, nonEmptyConjuncts, Tag.ofBoolean)
    }
    def not(e: ID)(implicit record: Total[ID] => ID): ID = {
//      assert(e.unfix.typ == Tag.ofBoolean)
      record(ComputationF(bool.Not, Seq(e), Tag.ofBoolean))
    }
    def implies(cond: ID, eff: ID)(implicit record: Total[ID] => ID): ID = {
//      assert(cond.unfix.typ == Tag.ofBoolean && eff.unfix.typ == Tag.ofBoolean)
      val notCond = record(ComputationF(bool.Not, Seq(cond), Tag.ofBoolean))
      record(ComputationF(bool.Or, Seq(notCond, eff), Tag.ofBoolean))
    }
  }
  import Utils._

  var cacheMiss = 0
  var cacheHit = 0

  trait TreeNode[N[_]] {
    def children[K](n: N[K]): Iterable[K]
  }
  object TreeNode {
    implicit val totalInstance: TreeNode[Total] = new TreeNode[Total] {
      override def children[A](fa: Total[A]): Iterable[A] = fa match {
        case ComputationF(_, args, _) => args
        case _: CstF[A]               => Nil
        case _: InputF[A]             => Nil
        case ITEF(c, t, f, _)         => Seq(c, t, f)
        case ProductF(as, _)          => as
      }
    }

    implicit val exprInstance: TreeNode[ExprF] = new TreeNode[ExprF] {
      override def children[A](fa: ExprF[A]): Iterable[A] = fa match {
        case Partial(value, condition, typ) => Seq(value, condition)
        case OptionalF(value, present, typ) => Seq(value, present)
        case PresentF(v)                    => Seq(v)
        case ValidF(v)                      => Seq(v)
        case x: Total[A]                    => totalInstance.children(x)
      }
    }
  }

  trait ILazyTree[K, F[_], Opt[_]] {
    type ID = Int
    def getExt(k: K): Opt[F[ID]]
    def getInternalID(k: K): Opt[ID]
    def getInt(i: ID): F[ID]

    def map[G[_]](f: F[Int] => G[Int])(implicit fOpt: Functor[Opt]): ILazyTree[K, G, Opt] = new MappedLazyTree(f, this)
  }

  class MappedLazyTree[K, F[_], G[_], Opt[_] : Functor](f: F[Int] => G[Int], mapped: ILazyTree[K, F, Opt]) extends ILazyTree[K,G, Opt] {
    private val memo = mutable.HashMap[ID, G[ID]]()

    override def getExt(k: K): Opt[G[ID]] = {
      getInternalID(k).map(getInt)
    }

    override def getInternalID(k: K): Opt[ID] = mapped.getInternalID(k)

    override def getInt(i: ID): G[ID] = memo.getOrElseUpdate(i, f(mapped.getInt(i)))
  }


  case class RootedLazyTree[K, F[_], Opt[_] : Functor](root: Int, tree: ILazyTree[K,F, Opt]) {
    def map[G[_]](f: F[Int] => G[Int]): RootedLazyTree[K,G, Opt] = RootedLazyTree(root, tree.map(f))

    def nodes(implicit tn: TreeNode[F]): Seq[(tree.ID, F[tree.ID])] = {
      val queue = mutable.Stack[tree.ID]()
      val visited = mutable.HashSet[tree.ID]()
      val result = mutable.ArrayBuffer[(tree.ID, F[tree.ID])]()
      queue.push(root)

      while(queue.nonEmpty) {
        val cur = queue.pop()
        val fcur = tree.getInt(cur)
        if(tn.children(fcur).forall(visited)) {
          visited += cur
          result += ((cur, fcur))
        } else {
          queue.push(cur)
          queue.pushAll(tn.children(fcur))
        }
      }
      result.toList
    }
  }

//  class LazyTree2[K, F[_]: TreeNode: Functor, G[_]](f: K => F[K],
//                                                   g: (G[ID] => ID) => F[G[ID]] => G[ID]) extends ILazyTree[K,G] {

//  class LazyTree[K, F[_]: TreeNode: Functor, G[_]](f: K => F[K],
//                                                   g: (G[ID] => ID) => F[G[ID]] => G[ID]) extends ILazyTree[K,G] {
//    private val treeNode = implicitly[TreeNode[F]]
//    private val functor = implicitly[Functor[F]]
//
//    private val idsMap = mutable.HashMap[K, ID]()
//    private val repMap = mutable.ArrayBuffer[G[ID]]() // ID => G[ID]
//
//    private val g2: F[G[ID]] => G[ID] = g(ga => {
//      val id = repMap.size
//      repMap += ga
//      id
//    })
//
//    @inline private def processed(k: K): Boolean = idsMap.contains(k)
//
//    def get(k: K): G[ID] = {
//      val queue = mutable.Queue[K]()
//      queue += k
//      while(queue.nonEmpty) {
//        val cur = queue.dequeue()
//        val fk = f(k)
//        if(treeNode.children(fk).forall(processed)) {
//          val fg = functor.map(fk)(id => repMap(idsMap(id)))
//          val g: G[ID] = g2(fg)
//          val id: ID = repMap.size
//          repMap += g
//          idsMap += ((k, id))
//        } else {
//          queue ++= treeNode.children(fk)
//          queue += k
//        }
//      }
//      repMap(idsMap(k))
//    }
//
//    override def getExt(k: K): G[ID] = get(k)
//
//    override def getInt(i: ID): G[ID] = repMap(i)
//  }

  abstract class TreeGenerator[K, F[_] : Functor : TreeNode] {
    def gen(k: K) : F[K]
//    def rec(fk: F[K]) : K

    private val treeNode = implicitly[TreeNode[F]]
    private val F = implicitly[Functor[F]]

    def cata[G](key: K)(f: F[G] =>  G) : G = {
      val trees = mutable.HashMap[K, G]()

      val queue = mutable.Stack[K]()
      queue.push(key)

      while(queue.nonEmpty) {
        val cur = queue.pop()
        val fk = gen(cur)
//        println(s"$cur (${queue.size}) -- $fk -- ${treeNode.children(fk)}")
        if(treeNode.children(fk).forall(k => trees.contains(k))) {
          val tree = f(F.map(fk)(id => trees(id)))
          trees += ((cur, tree))
        } else {
          queue.push(cur)
          queue.pushAll(treeNode.children(fk))
        }
      }
      trees(key)
    }
  }
  class TreeGenWithMap[ID, F[_], G[_] : Functor : TreeNode](f: F[ID] => G[ID], base: TreeGenerator[ID, F]) extends TreeGenerator[ID, G] {
    private val fw = mutable.HashMap[ID, G[ID]]()
//    private val bw = mutable.HashMap[G[ID], ID]()

    override def gen(k: ID): G[ID] =
      if(fw.contains(k))
        fw(k)
      else {
        val gk = f(base.gen(k))
        fw += ((k, gk))
        gk
      }
  }

  case class TreeWithRoot[K, F[_]](root: K, treeGenerator: TreeGenerator[K,F]) {
    def cata[G](f: F[G] => G): G = treeGenerator.cata(root)(f)
    def tree: Fix[F] = treeGenerator.cata[Fix[F]](root)(n => Fix(n))

    def map[G[_] : Functor : TreeNode](f: F[K] => G[K]): TreeWithRoot[K, G] = TreeWithRoot(root, new TreeGenWithMap(f, treeGenerator))
  }

  class LazyTreeSpec[K](f: K => ExprF[K], g: (Total[ID] => ID) => ExprF[IR[ID]] => IR[ID]) {
    private val treeNode = implicitly[TreeNode[ExprF]]
    private val functor = implicitly[Functor[ExprF]]

    private val idsMap = mutable.HashMap[K, IR[ID]]()
    private val repMap = mutable.ArrayBuffer[Total[ID]]() // ID => Total[ID]
    private val memo = mutable.HashMap[Total[ID], ID]()

    private def getID(e: Total[ID]): ID = {
      if(memo.contains(e))
        memo(e)
      else {
        val id = repMap.size
        repMap += e
        memo += ((e, id))
        id
      }

    }

    private val g2: ExprF[IR[ID]] => IR[ID] = g(ga => {
      getID(ga)
    })

    @inline private def processed(k: K): Boolean = idsMap.contains(k)

    def get(i: ID): Total[ID] = repMap(i)

    def get(key: K): IR[ID] = {
      val queue = mutable.Stack[K]()
      queue.push(key)

      while(queue.nonEmpty) {
        val cur = queue.pop()
        val fk = f(cur)
//        println(s"$cur (${queue.size}) -- $fk -- ${treeNode.children(fk)}")
        if(treeNode.children(fk).forall(processed)) {
          val fg = functor.map(fk)(id => idsMap(id))
          val g: IR[ID] = g2(fg)
          idsMap += ((cur, g))
        } else {
          queue.push(cur)
          queue.pushAll(treeNode.children(fk))
        }
      }
      idsMap(key)
    }

    def tree(key: ID): TreeWithRoot[ID, Total] = {
      val treeNode = implicitly[TreeNode[Total]]
      val F = implicitly[Functor[Total]]

      val trees = mutable.HashMap[ID, Fix[Total]]()

      val queue = mutable.Stack[ID]()
      queue.push(key)

      while(queue.nonEmpty) {
        val cur = queue.pop()
        val fk = repMap(cur)
//        println(s"$cur (${queue.size}) -- $fk -- ${treeNode.children(fk)}")
        if(treeNode.children(fk).forall(k => trees.contains(k))) {
          val tree = F.map(fk)(id => trees(id))
          trees += ((cur, tree))
        } else {
          queue.push(cur)
          queue.pushAll(treeNode.children(fk))
        }
      }
      trees(key)
      val x = new TreeGenerator[ID, Total] {
        override def gen(k: ID): Total[ID] = repMap(k)
      }
      TreeWithRoot(key, x)
    }
  }

  // X => ExprF[X]
  // ExprF[IR] => IR
  // X => IR ?

  case class IR[A](value: A, present: A, valid: A)

  def compiler[X](record: Total[ID] => ID): ExprF[IR[ID]] => IR[ID] = x => {
    implicit val implRec = record
    val ir = x match {
      case x: InputF[_] => IR(record(x), and(), and())
      case x: CstF[_]   => IR(record(x), and(), and())
      case ComputationF(f, args, t) =>
        IR(
          value = record(ComputationF(f, args.map(a => a.value), t)),
          present = and(args.map(_.present): _*),
          valid = and(args.map(_.valid): _*)
        )
      case ProductF(members, t) =>
        IR(
          value = record(ProductF(members.map(a => a.value), t)),
          present = and(members.map(_.present): _*),
          valid = and(members.map(_.valid): _*)
        )
      case ITEF(cond, onTrue, onFalse, t) =>
        IR(
          value = record(ITEF(cond.value, onTrue.value, onFalse.value, t)),
          present = and(cond.present,
                        implies(cond.value, onTrue.present),
                        implies(not(cond.value), onFalse.present)),
          valid = and(cond.valid,
                      implies(cond.value, onTrue.valid),
                      implies(not(cond.value), onFalse.valid))
        )
      case OptionalF(value, present, _) =>
        IR(
          value = value.value,
          present = and(value.present, present.present, present.value),
          valid = and(present.valid, value.valid)
        )
      case PresentF(opt) =>
        IR(
          value = opt.present,
          present = and(),
          valid = opt.valid
        )
      case ValidF(part) =>
        IR(
          value = part.valid,
          present = part.present,
          valid = and()
        )
      case Partial(value, condition, tpe) =>
        IR(
          value = value.value,
          present = value.present,
          valid =
            and(value.valid, implies(condition.present, and(condition.value, condition.valid)))
        )
    }
    ir
  }

  def optimizer(tot: Fix[Total]): Fix[Total] =
    dahu.recursion.Recursion
      .cata[Total, Fix[Total]](dahu.model.compiler.Optimizations.simplificationAlgebra)(tot)


  def encode[X](root: X, coalgebra: FCoalgebra[ExprF, X], optimize: Boolean = true): RootedLazyTree[X, Total, cats.Id] = {
    val lt = new LazyTreeSpec[X](coalgebra, compiler)
    val x = lt.get(root)

//    println(x)
//
//    lt.tree(x.valid)
//      .cata[Fix[Total]](node => Fix(node))

    val totalTrees = new ILazyTree[X, Total, cats.Id] {
      override def getExt(k: X): Total[ID] = lt.get(lt.get(k).value)
      override def getInt(i: ID): Total[ID] = lt.get(i)
      override def getInternalID(k: X): ID = lt.get(k).value
    }
    val satRoot = lt.get(root).valid
    RootedLazyTree(satRoot, totalTrees)
//    val cache = mutable.Map[ExprF[IR], IR]()
//    val ir = Recursion.hylo(coalgebra, compiler(cache, optimize = optimize))(root)
//
//    println(s"MISS / HIT : $cacheMiss / $cacheHit")
//
//        println(Algebras.format(ir.valid))
//    if(optimize)
//      optimizer(ir.valid)
//    else
//      ir.valid
  }

}
