package dahu.model.problem

import cats.Functor
import cats.implicits._
import dahu._
import dahu.model.functions._
import dahu.model.input.Anonymous
import dahu.model.ir._
import dahu.model.math._
import dahu.model.math.obj.Unboxed
import dahu.model.problem.SatisfactionProblem.{ILazyTree, RootedLazyTree, TreeNode}
import dahu.model.types._
import dahu.utils.SFunctor
import dahu.utils.Vec.Vec1
import dahu.utils.errors._

import scala.collection.mutable
import scala.reflect.ClassTag

class IntBoolSatisfactionProblem[X](val ast: RootedLazyTree[X, Total, cats.Id]) {

  /** Methods and variables in Internal are public so that there is no escape of private types.
    * However they are meant for internal use only are are subject to change. */
  object Internal {

    //    type K = builder.K

    sealed trait CellOpt[K]

    case class SupportedInput[K](value: InputF[K]) extends CellOpt[K] {
      require(value.typ == Tag.ofBoolean)
    }

    case class CompatibleInput[K](value: InputF[K], t: TagIsoInt[_]) extends CellOpt[K] {}

    case class SupportedConstant[K](value: CstF[K]) extends CellOpt[K] {
      require(value.typ == Tag.ofBoolean)
      require(value.value.isInstanceOf[Boolean])

    }

    case class CompatibleConstant[K](value: CstF[K], t: TagIsoInt[_]) extends CellOpt[K] {
      require(value.value.isInstanceOf[Int])
    }

    case class IntermediateExpression[K](value: ComputationF[K]) extends CellOpt[K] {}

    case object Unsupported extends CellOpt[Nothing]

    def unsupported[K](): CellOpt[K] = Unsupported.asInstanceOf[CellOpt[K]]

    private val supportedFunctions = Set[Fun[_]](int.Add,
                                                 int.LEQ,
                                                 int.EQ,
                                                 int.Times,
                                                 int.Negate,
                                                 int.Min,
                                                 bool.And,
                                                 bool.Or,
                                                 bool.XOr,
                                                 bool.Not)

    abstract class LazyTree[K, F[_]: TreeNode: SFunctor, G[_], Opt[_]: Functor](
        orig: ILazyTree[K, F, Opt])
        extends ILazyTree[K, G, Opt] {

      def g(rec: G[ID] => ID)(prev: ID => G[ID])(node: F[ID]): G[ID]

      private val treeNode = implicitly[TreeNode[F]]
      private val functor = implicitly[SFunctor[F]]

      private val idsMap = mutable.HashMap[orig.ID, ID]()
      private val repMap = mutable.ArrayBuffer[G[ID]]() // ID => G[ID]

      def rec(ga: G[ID]): ID = {
        val id = repMap.size
        repMap += ga
        id
      }

      private val g2: F[ID] => G[ID] = {
        val prev: ID => G[ID] = repMap(_)
        g(rec)(prev)
      }

      @inline private def processed(k: orig.ID): Boolean = {
        assert(!idsMap.contains(k) || repMap.size > idsMap(k))
        idsMap.contains(k)
      }

      def get(k: K): Opt[G[ID]] = {
        orig.getInternalID(k).map(getFromOrigID)
      }

      def getFromOrigID(origID: orig.ID): G[ID] = {
        val queue = mutable.Stack[orig.ID]()
        def push(origID: orig.ID): Unit = {
          if(!processed(origID)) {
            queue.push(origID)
          }
        }
        push(origID)
        while(queue.nonEmpty) {
          val cur = queue.pop()
          if(!processed(cur)) {
            val fk = orig.getInt(cur)
            if(treeNode.children(fk).forall(processed)) {
              val fg: F[ID] = functor.smap(fk)(id => idsMap(id))
              val g: G[ID] = g2(fg)
              val id = rec(g)
              idsMap += ((cur, id))
            } else {
              push(cur)
              treeNode.children(fk).foreach(push)
            }
          }
        }
        repMap(idsMap(origID))
      }

      override def getExt(k: K): Opt[G[ID]] = get(k)

      override def getInt(i: ID): G[ID] = repMap(i)

      override def getInternalID(k: K): Opt[ID] = getExt(k).map(rec)
    }

    private def sup[X](e: CellOpt[X]) = e match {
      case Unsupported => false
      case _           => true
    }

    private def TRANS[K: ClassTag](rec: CellOpt[K] => K)(prev: K => CellOpt[K])(
        node: Total[K]): CellOpt[K] = {
      def and(conjuncts: CellOpt[K]*): CellOpt[K] = {
        IntermediateExpression(
          ComputationF(bool.And, conjuncts.toSeq.map(x => rec(x)), Tag.ofBoolean)
        )
      }

      def leq(lhs: CellOpt[K], rhs: CellOpt[K]): CellOpt[K] = {
        IntermediateExpression(
          ComputationF(int.LEQ, Seq(lhs, rhs).map(x => rec(x)), Tag.ofBoolean)
        )
      }

      def cst(n: Int): CellOpt[K] = CompatibleConstant(CstF(Value(n), Tag.ofInt), Tag.ofInt)

      node match {
        case x @ CstF(v, Tag.ofBoolean) => SupportedConstant(x)
        case x @ CstF(v, t: TagIsoInt[_]) =>
          CompatibleConstant(CstF(Value(t.toIntUnsafe(v)), Tag.ofInt), t)
        case x @ InputF(name, Tag.ofBoolean)   => SupportedInput(x)
        case x @ InputF(name, t: TagIsoInt[_]) => CompatibleInput(InputF(name, Tag.ofInt), t)
        case x @ ComputationF(f, args, t: TagIsoInt[_])
            if supportedFunctions.contains(f) && args.forall(x => sup(prev(x))) =>
          IntermediateExpression(ComputationF(f, args, t))
        case x @ ComputationF(wf: WrappedFunction, args, t: TagIsoInt[_])
            if supportedFunctions.contains(wf.f) && args.forall(x => sup(prev(x))) =>
          TRANS(rec)(prev)(ComputationF(wf.f, args, t)) // unwrap and retry

        case x @ ComputationF(f: Unboxed[_], Vec1(arg), t) =>
          prev(arg) // unbox operation, use the previous cell

        case x =>
          x.typ match {
            case Tag.ofBoolean =>
              SupportedInput(InputF(Anonymous(), Tag.ofBoolean))
            case t: TagIsoInt[_] =>
              CompatibleInput(InputF(Anonymous(), Tag.ofInt), t)
            case _ =>
              unsupported()
          }
      }
    }

    val lt = new LazyTree[X, Total, CellOpt, cats.Id](ast.tree) {
      override def g(rec: CellOpt[ID] => ID)(prev: ID => CellOpt[ID])(
          node: Total[ID]): CellOpt[ID] =
        TRANS(rec)(prev)(node)
    }

    def leq(lhs: CellOpt[lt.ID], rhs: CellOpt[lt.ID]): CellOpt[lt.ID] = {
      IntermediateExpression(
        ComputationF(int.LEQ, Seq(lhs, rhs).map(x => lt.rec(x)), Tag.ofBoolean)
      )
    }

    def cst[K](n: Int): CellOpt[K] = CompatibleConstant(CstF(Value(n), Tag.ofInt), Tag.ofInt)

    private def gatherConstraints(k: lt.ID): Set[lt.ID] = {
      val cs = mutable.HashSet[lt.ID]()
      val visited = mutable.HashSet[lt.ID]()
      val stack = mutable.Stack[lt.ID]()
      def push(i: lt.ID): Unit = { if(!visited.contains(i)) stack.push(i) }

      push(k)
      while(stack.nonEmpty) {
        val cur = stack.pop()
        visited += cur
        lt.getInt(cur) match {
          case i @ CompatibleInput(_, tpe) =>
            cs += lt.rec(leq(cst(tpe.min), i))
            cs += lt.rec(leq(i, cst(tpe.max)))
          case IntermediateExpression(e) =>
            e.args.foreach(push)
          case _ =>
        }
      }
      cs.toSet
    }

    //
    // the root value, is the conjunction of the original root and all constraints placed on inputs.
    private val internalPrevRoot = lt.rec(lt.getFromOrigID(ast.root))
    private val rootValue = IntermediateExpression(
      ComputationF(bool.And,
                   (gatherConstraints(internalPrevRoot) + internalPrevRoot).toSeq,
                   bool.And.tpe))
    val root: lt.ID = lt.rec(rootValue)

//    val optTree: RootedLazyTree[X, CellOpt, cats.Id] = RootedLazyTree(root, lt)

    type OptTotal[T] = Option[Total[T]]

    val partialTree = new ILazyTree[X, Total, Option] {
      val t = lt.map[OptTotal]({
        case IntermediateExpression(e) => Some(e)
        case CompatibleInput(v, _)     => Some(v)
        case SupportedInput(v)         => Some(v)
        case CompatibleConstant(v, _)  => Some(v)
        case SupportedConstant(v)      => Some(v)
        case x if x == Unsupported     => None
      })
      override def getExt(k: X): Option[Total[ID]] = t.getExt(k)

      override def getInternalID(k: X): Option[ID] = getExt(k).map(_ => t.getInternalID(k))

      override def getInt(i: ID): Total[ID] = t.getInt(i) match {
        case Some(e) => e
        case None    => unexpected
      }
    }

    val tree = RootedLazyTree(root, partialTree)
  }

  val tree: RootedLazyTree[X, Total, Option] = Internal.tree
//
//    val coalg: K => Option[Total[K]] = k => {
//      val res = builder(k) match {
//        case IntermediateExpression(e) => Some(e)
//        case CompatibleInput(v, _)     => Some(v)
//        case SupportedInput(v)         => Some(v)
//        case CompatibleConstant(v, _)  => Some(v)
//        case SupportedConstant(v)      => Some(v)
//        case Unsupported               => None
//      }
//      res
//    }
//
//    // from builder, gather all expression that can be represented (i.e. int or bool)
//    val tmp =
//      ArrayMap
//        .buildWithKey(builder.domain)(k => coalg(k))
//        .collect { case Some(v) => v }

//
////  type K = Internal.tmp.K
//
//  // an algebra X => Total[X], where Total[X] is guaranteed to be representable.
//  val algebra: ArrayMap.Aux[K, Total[K]] = Internal.tmp.asInstanceOf[ArrayMap.Aux[K, Total[K]]]
//
//  assert(algebra.isInDomain(Internal.root))
//  val root: K = Internal.root.asInstanceOf[K]

//  def tree: RootedLazyTree[X, Total] = {
//    new RootedLazyTree[X, Total](ast.root, )
//  }
}
