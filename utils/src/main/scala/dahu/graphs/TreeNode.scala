package dahu.graphs

import scala.collection.immutable.Iterable

trait TreeNode[-N[_]] {

  def children[K](n: N[K]): Iterable[K]

  def foreachChild[K](n: N[K])(f: K => Unit): Unit

  def forallChildren[K](n: N[K])(f: K => Boolean): Boolean = {
    try {
      foreachChild(n) { c =>
        if(!f(c)) throw TreeNode.break
      }
      true
    } catch {
      case e if e eq TreeNode.break => false
    }
  }
  def existsChild[K](n: N[K])(f: K => Boolean): Boolean = {
    try {
      foreachChild(n) { c =>
        if(f(c)) throw TreeNode.break
      }
      false
    } catch {
      case e if e eq TreeNode.break => true
    }
  }

  def childrenFoldLeft[K, B](n: N[K])(orig: B)(f: (B, K) => B): B = {
    var acc = orig
    foreachChild(n) { c =>
      acc = f(acc, c)
    }
    acc
  }
}
object TreeNode {
  private val break: Throwable = new Throwable()

  def apply[N[_]](implicit instance: TreeNode[N]): TreeNode[N] = instance

  implicit class TreeNodeOps[N[_], K](private val lhs: N[K]) extends AnyVal {
    def children(implicit TN: TreeNode[N]): Iterable[K] = TN.children(lhs)

    def foreachChild(f: K => Unit)(implicit TN: TreeNode[N]): Unit = TN.foreachChild(lhs)(f)
    def forallChildren(f: K => Boolean)(implicit TN: TreeNode[N]): Boolean =
      TN.forallChildren(lhs)(f)

    def existsChild(f: K => Boolean)(implicit TN: TreeNode[N]): Boolean =
      TN.existsChild(lhs)(f)

    def childrenFolLeft[B](b: B)(f: (B, K) => B)(implicit TN: TreeNode[N]): B =
      TN.childrenFoldLeft(lhs)(b)(f)

    def childrenContains(e: K)(implicit TN: TreeNode[N]): Boolean = !TN.forallChildren(lhs)(e != _)
  }
}
