package dahu.constraints

package object interval {

  trait IntervalTag { self: Long => }
  type Interval = Long with IntervalTag

  object Interval {
    /** Unique representation of the empty interval as [1, 0].
      * This is necessary to make sure the == is the same for Long and Interval. */
    val empty: Interval = ((1L << 32) | (0L & 0xFFFFFFFFL)).asInstanceOf[Interval]
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
      lb <= rhs.lb && rhs.ub <= ub

    def show: String =
      if(isEmpty) "{}"
      else if(isSingleton) lb.toString
      else s"[$lb, $ub]"
  }
}
