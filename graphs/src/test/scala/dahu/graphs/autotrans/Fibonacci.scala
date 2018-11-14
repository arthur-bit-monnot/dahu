package dahu.graphs.autotrans
import dahu.graphs.TreeNode
import dahu.utils.uoption.{UNone, UOption, USome}
import dahu.utils.{ClassTag, SFunctor}

import scala.collection.immutable

object Fibonacci extends App {

  sealed trait Comp[X]
  case class Fib[X](a: Int) extends Comp[X]
  case class Sum[X](a: X, b: X) extends Comp[X]
  case class Num[X](x: BigInt) extends Comp[X]

  implicit val sfunctor: SFunctor[Comp] = new SFunctor[Comp] {
    override def smap[@specialized A, @specialized B: ClassTag](fa: Comp[A])(f: A => B): Comp[B] =
      fa match {
        case Sum(a, b) => Sum(f(a), f(b))
        case Num(i)    => Num(i)
        case Fib(i)    => Fib(i)
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

  val trans = new Transformation[Comp] {
    override def dependenceDepth: Int = 1
    override def transformation[I <: Int](retrieve: I => Comp[I],
                                          record: Comp[I] => I): Comp[I] => UOption[Comp[I]] = {
      case Fib(n) if n < 0 => ???
      case Fib(0)          => USome(Num(0))
      case Fib(1)          => USome(Num(1))
      case Fib(n) =>
        val fa = record(Fib(n - 1))
        val fb = record(Fib(n - 2))
        USome(Sum(fa, fb))
      case Num(_) => UNone
      case Sum(a, b) =>
        (retrieve(a), retrieve(b)) match {
          case (Num(na), Num(nb)) => USome(Num(na + nb))
          case x =>
            UNone
        }
    }
  }

  for(i <- 0 until 1000) {
    val g = new AutoTransformation[Comp](trans)

    val i = g.deepRecord(Fib(1000))
    println(g.extract(i))
  }

}
