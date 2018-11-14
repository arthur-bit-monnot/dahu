package dahu.graphs
import dahu.utils.{ClassTag, SFunctor}

import scala.collection.immutable

object Fibonacci extends App {

  sealed trait Comp[X]
  case class Sum[X](a: X, b: X) extends Comp[X]
  case class Num[X](x: Int) extends Comp[X]

  implicit val sfunctor: SFunctor[Comp] = new SFunctor[Comp] {
    override def smap[@specialized A, @specialized B: ClassTag](fa: Comp[A])(f: A => B): Comp[B] =
      fa match {
        case Sum(a, b) => Sum(f(a), f(b))
        case Num(i)    => Num(i)
      }
  }
  implicit val treeNode: TreeNode[Comp] = new TreeNode[Comp] {
    override def children[K](n: Comp[K]): immutable.Iterable[K] = n match {
      case Sum(a, b) => immutable.Iterable(a, b)
      case _         => immutable.Iterable.empty
    }
    override def foreachChild[K](n: Comp[K])(f: K => Unit): Unit = n match {
      case Sum(a, b) => f(a); f(b)
      case _         =>
    }
  }
  val start = System.currentTimeMillis()
  for(i <- 0 to 500) {
    val tree = LazyTree.parse[Int, Comp](4, {
      case 0 => Num[Int](0)
      case 1 => Num[Int](1)
      case i => Sum(i - 1, i - 2)
    })
    val fibo = tree.tree.cata[BigInt] {
      case Num(i)    => i
      case Sum(a, b) => a + b
    }

    fibo.get(10)
  }
  val end = System.currentTimeMillis()
  println(end - start)
}
