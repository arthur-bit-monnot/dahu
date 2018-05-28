package dahu.model.problem

import dahu.model.functions._
import dahu.model.input.Ident
import dahu.model.ir._
import dahu.model.math._
import dahu.model.types._
import dahu.utils.SFunctor
import dahu.utils.Vec.Vec1
import dahu.utils.errors._

import scala.collection.mutable
import scala.reflect.ClassTag

class IntBoolSatisfactionProblem[X <: Int](rootNode: X, coalg: X => Total[X]) {

  /** Methods and variables in Internal are public so that there is no escape of private types.
    * However they are meant for internal use only are are subject to change. */
  object Internal {
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

    private def sup[X](e: CellOpt[X]) = e match {
      case Unsupported => false
      case _           => true
    }

    private def isUnbox(f: Fun[_]): Boolean = f match {
      case fun: Fun1[_, _] =>
        fun.inType match {
          case t: TagIsoInt[_] => t.unbox == fun
          case _               => false
        }
      case _ => false
    }
    private def isBox(f: Fun[_]): Boolean = f match {
      case fun: Fun1[_, _] =>
        fun.outType match {
          case t: TagIsoInt[_] => t.box == fun
          case _               => false
        }
      case _ => false
    }

    private def TRANS[K: ClassTag](rec: CellOpt[K] => K)(prev: K => CellOpt[K])(
        node: Total[K]): K = {

      val res: CellOpt[K] = node match {
        case x @ CstF(v, Tag.ofBoolean) => SupportedConstant(x)
        case x @ CstF(v, t: TagIsoInt[_]) =>
          CompatibleConstant(CstF(Value(t.toIntUnsafe(v)), Tag.ofInt), t)
        case x @ InputF(name, Tag.ofBoolean)   => SupportedInput(x)
        case x @ InputF(name, t: TagIsoInt[_]) => CompatibleInput(InputF(name, Tag.ofInt), t)
        case x @ ComputationF(f, args, t: TagIsoInt[_])
            if supportedFunctions.contains(f) && args.forall(x => sup(prev(x))) =>
          IntermediateExpression(ComputationF(f, args, t))
        case x @ ComputationF(f: Fun1[_, _], Vec1(arg), t: TagIsoInt[_])
            if isUnbox(f) && sup(prev(arg)) => //TODO double check
          prev(arg)
        case x @ ComputationF(f: Fun1[_, _], Vec1(arg), t: TagIsoInt[_])
            if isBox(f) && sup(prev(arg)) => //TODO double check
          prev(arg)
        case x =>
          val x2 = SFunctor[Total].smap(x)(prev)
          dahu.utils.debug.warning(s"unsupported: $x2")
          x.typ match {
            case Tag.ofBoolean =>
              SupportedInput(InputF(Ident.anonymous(), Tag.ofBoolean))
            case t: TagIsoInt[_] =>
              CompatibleInput(InputF(Ident.anonymous(), Tag.ofInt), t)
            case _ =>
              unsupported()
          }
      }
      rec(res)
    }
    val lt =
      LazyTree.parseGen[X, Total, CellOpt](rootNode, coalg, ctx => TRANS(ctx.record)(ctx.retrieve))

    def leq(lhs: CellOpt[lt.ID], rhs: CellOpt[lt.ID]): CellOpt[lt.ID] = {
      IntermediateExpression(
        ComputationF(int.LEQ, Seq(lhs, rhs).map(x => lt.record(x)), Tag.ofBoolean)
      )
    }

    def cst[K](n: Int): CellOpt[K] = CompatibleConstant(CstF(Value(n), Tag.ofInt), TagIsoInt.ofInt)

    private def gatherConstraints(k: lt.ID): Set[lt.ID] = {
      val cs = mutable.HashSet[lt.ID]()
      val visited = mutable.HashSet[lt.ID]()
      val stack = mutable.Stack[lt.ID]()

      def push(i: lt.ID): Unit = {
        if(!visited.contains(i)) stack.push(i)
      }

      push(k)
      while(stack.nonEmpty) {
        val cur = stack.pop()
        visited += cur
        lt.internalCoalgebra(cur) match {
          case i @ CompatibleInput(_, tpe) =>
            cs += lt.record(leq(cst(tpe.min), i))
            cs += lt.record(leq(i, cst(tpe.max)))
          case IntermediateExpression(e) =>
            e.args.foreach(push)
          case _ =>
        }
      }
      cs.toSet
    }

    // the root value, is the conjunction of the original root and all constraints placed on inputs.
    private val internalPrevRoot = lt.record(lt.getExt(rootNode))
    private val rootValue = IntermediateExpression(
      ComputationF(bool.And,
                   (gatherConstraints(internalPrevRoot) + internalPrevRoot).toSeq,
                   bool.And.tpe))
    val root: lt.ID = lt.record(rootValue)

    type OptTotal[T] = Option[Total[T]]
    val t = lt.mapInternal[OptTotal]({
      case IntermediateExpression(e) => Some(e)
      case CompatibleInput(v, _)     => Some(v)
      case SupportedInput(v)         => Some(v)
      case CompatibleConstant(v, _)  => Some(v)
      case SupportedConstant(v)      => Some(v)
      case x if x == Unsupported     => None
    })
    val partialTree = new IlazyForest[X, Total, Option, lt.ID] {
      override def getTreeRoot(k: X): Option[lt.ID] = {
        if(k == rootNode)
          Some(root) // we modified the root to account for additional constraints,
        else
          (t.getExt(k): Option[Total[lt.ID]]) match {
            case Some(_) => Some(t.getTreeRoot(k))
            case None    => None
          }
      }
      override def internalCoalgebra(i: lt.ID): Total[lt.ID] = t.internalCoalgebra(i) match {
        case Some(e) => e
        case None    => unexpected
      }
    }

    val tree = LazyTree(partialTree)(rootNode)
  }

  val tree: LazyTree[X, Total, Option, _] = Internal.tree
}
