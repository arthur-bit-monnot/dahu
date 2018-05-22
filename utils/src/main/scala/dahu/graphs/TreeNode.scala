package dahu.graphs

import scala.collection.immutable.Iterable

trait TreeNode[-N[_]] {
  def children[K](n: N[K]): Iterable[K]
}
object TreeNode {
  def apply[N[_]](implicit instance: TreeNode[N]): TreeNode[N] = instance

  implicit class TreeNodeOps[N[_], K](private val lhs: N[K]) extends AnyVal {
    def children(implicit TN: TreeNode[N]): Iterable[K] = TN.children(lhs)
  }
}
