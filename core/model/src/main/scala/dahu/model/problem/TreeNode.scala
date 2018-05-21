package dahu.model.problem

import dahu.model.ir._
import scala.collection.immutable.Iterable

trait TreeNode[-N[_]] {
  def children[K](n: N[K]): Iterable[K]
}
object TreeNode {
  def apply[N[_]](implicit instance: TreeNode[N]): TreeNode[N] = instance

  implicit val totalInstance: TreeNode[Total] = new TreeNode[Total] {
    override def children[A](fa: Total[A]): Iterable[A] = fa match {
      case ComputationF(_, args, _) => args.toIterable
      case _: CstF[A]               => Iterable.empty
      case _: InputF[A]             => Iterable.empty
      case ITEF(c, t, f, _)         => Iterable(c, t, f)
      case ProductF(as, _)          => as.toIterable
    }
  }

  implicit val exprInstance: TreeNode[ExprF] = new TreeNode[ExprF] {
    override def children[A](fa: ExprF[A]): Iterable[A] = fa match {
      case Partial(value, condition, _)     => Iterable(value, condition)
      case OptionalF(value, present, _)     => Iterable(value, present)
      case PresentF(v)                      => Iterable(v)
      case ValidF(v)                        => Iterable(v)
      case DynamicF(params, f, _, _)        => Iterable(params, f)
      case DynamicProviderF(e, provided, _) => Iterable(e, provided)
      case x: Total[A]                      => totalInstance.children(x)
    }
  }

  implicit class TreeNodeOps[N[_], K](private val lhs: N[K]) extends AnyVal {
    def children(implicit TN: TreeNode[N]): Iterable[K] = TN.children(lhs)
  }
}
