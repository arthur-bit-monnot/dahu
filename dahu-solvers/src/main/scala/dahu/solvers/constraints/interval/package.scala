package dahu.constraints

import algebra.Order
import dahu.utils.Errors._
import dahu.utils.debug._

import scala.reflect.ClassTag

package object interval {

  trait IntIntervalTag { self: Long =>
  }
  type Interval = Long with IntIntervalTag

  import spire.implicits._
  implicit val classTag: ClassTag[Interval] =
    implicitly[ClassTag[Long]].asInstanceOf[ClassTag[Interval]]
  val lowerBoundOrder: Order[Interval] = (lhs: Interval, rhs: Interval) =>
    Order[Int].compare(lhs.lb, rhs.lb)
  val sizeOrder: Order[Interval] = (lhs: Interval, rhs: Interval) =>
    Order[Int].compare(lhs.size, rhs.size)

  object Interval {

    /** Unique representation of the empty interval as [1, 0].
      * This is necessary to make sure the == is the same for Long and Interval. */
    final val empty: Interval = ((1L << 32) | (0L & 0xFFFFFFFFL)).asInstanceOf[Interval]
    final val full: Interval = Interval(Integer.MIN_VALUE / 2 + 1, Integer.MAX_VALUE / 2 - 1)

    def apply(singleton: Int): Interval = apply(singleton, singleton)
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
    def size: Int = ub - lb + 1
    def values: Range = lb to ub

    def isSingleton: Boolean = size == 1
    def isEmpty: Boolean = lhs == Interval.empty

    def inter(rhs: Interval): Interval =
      Interval(math.max(lhs.lb, rhs.lb), math.min(lhs.ub, rhs.ub))

    /** Note: this is an approximation: [0,1] union [10, 20] will return [0, 20]. */
    def unionApproximation(rhs: Interval): Interval = approximation {
      Interval(math.min(lhs.lb, rhs.lb), math.max(lhs.ub, rhs.ub))
    }

    def shift(delta: Int): Interval =
      Interval(lb + delta, ub + delta)

    def plus(rhs: Interval): Interval = Interval(lb + rhs.lb, ub + rhs.ub)
    def minus(rhs: Interval): Interval = Interval(lb - rhs.ub, ub - rhs.lb)
    def -(rhs: Interval): Interval = minus(rhs)
    def negated: Interval = Interval(-ub, -lb)

    def contains(rhs: Interval): Boolean =
      if(rhs.isEmpty) true
      else lb <= rhs.lb && rhs.ub <= ub
    def strictlyContains(rhs: Interval): Boolean =
      if(!isEmpty && rhs.isEmpty) true
      else lb < rhs.lb && rhs.ub < ub

    def contains(value: Int): Boolean = lb <= value && value <= ub

    /** CAREFUL: this is an approximation.
      * In the case where `rhs` is fully contained in `lhs` the result will be `lhs`.
      * E.g. [0, 2] \ [1,1] will return [0, 2].
      * This is needed for efficiency to avoid splitting into two intervals. */
    def withoutApproximation(rhs: Interval): Interval =
      if(isEmpty) Interval.empty
      else if(rhs.contains(lhs)) Interval.empty // rhs fully covers lhs
      else if(rhs.ub < lb || ub < rhs.lb) lhs //disjoint
      else if(lhs.strictlyContains(rhs)) approximation { lhs } else if(rhs.lb <= lb && lb <= rhs.ub)
        Interval(rhs.ub + 1, ub)
      else if(rhs.ub >= ub && ub >= rhs.lb) Interval(lb, rhs.lb - 1)
      else unexpected(s"Unhandled case:  ${lhs.show} \\ ${rhs.show}")

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

  trait BoolIntervalTag { self: Interval =>
  }
  type BooleanDomain = Interval with BoolIntervalTag

  object BooleanDomain {
    final val True: BooleanDomain = Interval(1).asInstanceOf[BooleanDomain]
    final val False: BooleanDomain = Interval(0).asInstanceOf[BooleanDomain]
    final val Unknown: BooleanDomain = Interval(0, 1).asInstanceOf[BooleanDomain]
    final val empty: BooleanDomain = Interval.empty.asInstanceOf[BooleanDomain]

    def asBooleanDomains(doms: Array[Interval]): Array[BooleanDomain] = {
      assert3(doms.forall(Unknown.contains))
      doms.asInstanceOf[Array[BooleanDomain]]
    }
  }
  import BooleanDomain._
  implicit final class BooleanDomainOps(val lhs: BooleanDomain) extends AnyVal {

    def isFalse: Boolean = lhs == False
    def isTrue: Boolean = lhs == True

    def or(rhs: BooleanDomain): BooleanDomain =
      if(lhs.isEmpty || rhs.isEmpty) BooleanDomain.empty
      else if(lhs.isTrue || rhs.isTrue) True
      else if(lhs == False && rhs == False) False
      else Unknown

    def and(rhs: BooleanDomain): BooleanDomain =
      if(lhs.isEmpty || rhs.isEmpty) BooleanDomain.empty
      else if(lhs.isTrue && rhs.isTrue) True
      else if(lhs.isFalse || rhs.isFalse) False
      else Unknown
  }
}
