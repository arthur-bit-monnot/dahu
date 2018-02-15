package dahu.constraints

import algebra.Order

import scala.reflect.ClassTag

package object interval {

  trait IntervalTag { self: Long => }
  type Interval = Long with IntervalTag

  implicit val classTag: ClassTag[Interval] = ClassTag[Long].asInstanceOf[ClassTag[Interval]]
  val lowerBoundOrder: Order[Interval] = (lhs: Interval, rhs: Interval) => Order[Int].compare(lhs.lb, rhs.lb)
  val sizeOrder: Order[Interval] = (lhs: Interval, rhs: Interval) => Order[Int].compare(lhs.size, rhs.size)

  object Interval {
    /** Unique representation of the empty interval as [1, 0].
      * This is necessary to make sure the == is the same for Long and Interval. */
    final val empty: Interval = ((1L << 32) | (0L & 0xFFFFFFFFL)).asInstanceOf[Interval]
    final val True: Interval = Interval(1,1)
    final val False: Interval = Interval(0,0)
    def apply(lb: Int, ub: Int): Interval = {
      if(lb > ub) {
        empty
      } else {
        val l = lb.toLong << 32 | ub.toLong & 0xFFFFFFFFL
        l.asInstanceOf[Interval]
      }
    }
  }

  implicit final class IntervalOps(val lhs: Interval) extends AnyVal {
    def lb: Int = (lhs >> 32).toInt
    def ub: Int = lhs.toInt
    def size: Int = ub - lb +1
    def values: Range = lb to ub

    def isSingleton: Boolean = size == 1
    def isEmpty: Boolean = lhs == Interval.empty

    def intersection(rhs: Interval): Interval =
      Interval(math.max(lhs.lb, rhs.lb), math.min(lhs.ub, rhs.ub))

    def union(rhs: Interval): Interval =
      Interval(math.min(lhs.lb, rhs.lb), math.max(lhs.ub, rhs.ub))

    def shift(delta: Int): Interval =
      Interval(lb + delta, ub + delta)

    def contains(rhs: Interval): Boolean =
      if(rhs.isEmpty) true
      else lb <= rhs.lb && rhs.ub <= ub
    def strictlyContains(rhs: Interval): Boolean =
      if(!isEmpty && rhs.isEmpty) true
      else lb < rhs.lb && rhs.ub < ub

    def without(rhs: Interval): Interval =
      if(isEmpty) Interval.empty
      else if(rhs.contains(lhs)) Interval.empty
      else if(rhs.ub < lb || ub < rhs.lb) lhs
      else if(lhs.strictlyContains(rhs)) lhs
      else if(rhs.lb <= lb && lb < rhs.ub) Interval(rhs.ub+1, ub)
      else if(rhs.ub >= ub && ub > rhs.lb) Interval(lb, rhs.lb-1)
      else dahu.utils.Errors.unexpected(lhs.show + " " + rhs.show)

    def \(rhs: Interval): Interval = without(rhs)

    def min(rhs: Interval): Interval =
      if(isEmpty || rhs.isEmpty) Interval.empty
      else Interval(math.min(lb, rhs.lb), math.min(ub, rhs.ub))
    def max(rhs: Interval): Interval =
      if(isEmpty || rhs.isEmpty) Interval.empty
      else Interval(math.max(lb, rhs.lb), math.max(ub, rhs.ub))

    def show: String =
      if(isEmpty) "{}"
      else if(isSingleton) lb.toString
      else s"[$lb, $ub]"
  }
}
