package dahu.planning.model.common

import dahu.core.algebra.{BoolLike, Orderable}

sealed abstract class Interval[A] {
  val start: A
  val end: A

  def isLeftOpen: Boolean
  def isRightOpen: Boolean

  def map[B](f: A => B): Interval[B] = this match {
    case ClosedInterval(s, e)    => ClosedInterval(f(s), f(e))
    case LeftOpenInterval(s, e)  => LeftOpenInterval(f(s), f(e))
    case RightOpenInterval(s, e) => RightOpenInterval(f(s), f(e))
    case OpenInterval(s, e)      => OpenInterval(f(s), f(e))
  }

  def substituteWith[B](start: B, end: B): Interval[B] = this match {
    case _: ClosedInterval[A]    => ClosedInterval(start, end)
    case _: LeftOpenInterval[A]  => LeftOpenInterval(start, end)
    case _: RightOpenInterval[A] => RightOpenInterval(start, end)
    case _: OpenInterval[A]      => OpenInterval(start, end)
  }
}
object Interval {
  def point[A](pt: A): Interval[A] = ClosedInterval(pt, pt)

  implicit def orderedInstance[A](implicit O: Orderable[A]): Orderable.Aux[Interval[A], O.Bool] =
    new Orderable[Interval[A]] {
      type Bool = O.Bool
      override def BL: BoolLike[Bool] = O.BL
      override def leq(a: Interval[A], b: Interval[A]): Bool =
        O.leq(a.end, b.start)

      override def lt(a: Interval[A], b: Interval[A]): Bool =
        if(a.isRightOpen || b.isLeftOpen)
          O.leq(a.end, b.start)
        else
          O.lt(a.end, b.start)
    }
}

case class ClosedInterval[A](start: A, end: A) extends Interval[A] {
  override def toString: String = s"[$start, $end]"
  override def isLeftOpen: Boolean = false
  override def isRightOpen: Boolean = false
}

case class LeftOpenInterval[A](start: A, end: A) extends Interval[A] {
  override def toString: String = s"]$start, $end]"
  override def isLeftOpen: Boolean = true
  override def isRightOpen: Boolean = false
}

case class RightOpenInterval[A](start: A, end: A) extends Interval[A] {
  override def toString: String = s"[$start, $end["
  override def isLeftOpen: Boolean = false
  override def isRightOpen: Boolean = true
}

case class OpenInterval[A](start: A, end: A) extends Interval[A] {
  override def toString: String = s"]$start, $end["
  override def isLeftOpen: Boolean = true
  override def isRightOpen: Boolean = true
}
