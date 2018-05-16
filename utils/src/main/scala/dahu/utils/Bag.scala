package dahu.utils

import java.util.Optional

import dahu.utils.Bag.{Cons, ConsA}

import scala.collection.immutable.Iterable

/** A bag is an unordered collections, built from iterables.
  *
  * It aims at maximum structural sharing when
  *
  */
sealed trait Bag[+A] extends Iterable[A] {
  self =>
  def ++[B >: A](iterable: Iterable[B]): Bag[B] = Bag.Cons(iterable, this)

  def +[B >: A](item: B): Bag[B] = Bag.ConsA(item, this)

  // TODO: this is stack unsafe
  def map[B](f: A => B): Bag[B] = self match {
    case Bag.Cons(h, tl)  => Cons(h.map(f), tl.map(f))
    case Bag.ConsA(h, tl) => ConsA(f(h), tl.map(f))
    case Bag.Nil          => Bag.Nil
  }

  private def headIterator: Iterator[A] = self match {
    case Bag.Cons(h, _)  => h.iterator
    case Bag.ConsA(h, _) => Iterator(h)
    case Bag.Nil         => Iterator.empty
  }

  private def tailBag: Bag[A] = self match {
    case Bag.Cons(_, tl)  => tl
    case Bag.ConsA(_, tl) => tl
    case Bag.Nil          => null
  }

  override def iterator: Iterator[A] = new Iterator[A] {
    private var nextBag: Bag[A] = self.tailBag
    private var it = self.headIterator

    override def hasNext: Boolean = it.hasNext

    override def next(): A = {
      val ret = it.next()
      while(!it.hasNext && nextBag != null) {
        it = nextBag.headIterator
        nextBag = nextBag.tailBag
      }
      ret
    }
  }
}

object Bag {
  def apply[A](x: Iterable[A]): Bag[A] = Cons(x, Nil)
  def apply[A](a: A, as: A*): Bag[A] = ConsA(a, Cons(as.to, Nil))
  def fromIterables[A](its: Iterable[Iterable[A]]): Bag[A] =
    its.foldLeft[Bag[A]](empty)((acc, cur) => Cons(cur, acc))

  def empty: Bag[Nothing] = Nil

  case object Nil extends Bag[Nothing]

  final case class Cons[+A] private (lastElems: collection.Iterable[A], prev: Bag[A]) extends Bag[A]

  final case class ConsA[+A](item: A, prev: Bag[A]) extends Bag[A]

}
