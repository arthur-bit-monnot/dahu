package dahu.arrows

import scala.collection.mutable

trait PartialArrow[A, B] extends Arrow[A, Option[B]] {

}

trait MutableArrow[A, B] {
  def apply(a: A): B
}

class MutableInputs[A, B] {

  val known: mutable.Map[A, B] = mutable.Map()

  def update(a: A, b: B): Unit = known.update(a, b)

  def unset(a: A): Unit = known -= a

  def apply(a: A): Option[B] = known.get(a)

}

class ArrowFromInputs[A, B, C](val inputs: MutableInputs[A, B], val arrow: B ==> C) extends MutableArrow[A, Option[C]] {
  override def apply(a: A): Option[C] = inputs(a).map(arrow(_))
}