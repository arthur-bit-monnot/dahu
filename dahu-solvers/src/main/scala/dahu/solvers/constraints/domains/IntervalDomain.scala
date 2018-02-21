package dahu.constraints.domains

import dahu.utils.debug._

class IntervalDomain(val lb: Int, val ub: Int) extends IntDomain {
  require(ub < Integer.MAX_VALUE / 2 && lb > Integer.MIN_VALUE / 2,
          "To avoid overflow when computing size")

  override def size: Int = math.max(0, ub - lb + 1)

  override def values: Iterable[Int] = lb to ub

  override def intersection(other: IntDomain): IntDomain = other match {
    case other: IntervalDomain =>
      IntervalDomain(math.max(lb, other.lb), math.min(ub, other.ub))
    case other: EnumeratedDomain =>
      slow { new EnumeratedDomain(other.values.filter(v => lb <= v && v <= ub)) }
    case _ =>
      super.intersection(other)
  }

  private def strictlyContains(other: IntervalDomain) = lb < other.lb && other.ub < ub
  private def contains(other: IntervalDomain) = lb <= other.lb && other.ub <= ub

  override def without(other: IntDomain): IntDomain = other match {
    case x if x.isSingleton =>
      without(x.head)

    case other: IntervalDomain if !strictlyContains(other) =>
      if(emptyIntersection(other))
        this
      else if(other.contains(this))
        EmptyDomain
      else if(other.lb < ub)
        IntervalDomain(lb, other.lb - 1)
      else {
        assert(other.ub > lb)
        IntervalDomain(other.ub + 1, ub)
      }

    case _ =>
      super.without(other)
  }

  override def without(toRm: Int): IntDomain = {
    if(toRm < lb || toRm > ub) this
    else if(this.isSingleton) EmptyDomain
    else if(toRm == lb) IntervalDomain(lb + 1, ub)
    else if(toRm == ub) IntervalDomain(lb, ub - 1)
    else super.without(toRm) // value in the interval, resort to default method
  }

  override def contains(value: Int): Boolean = lb <= value && value <= ub

  override def wizz(value: Int): IntDomain = {
    if(value == lb - 1)
      IntervalDomain(lb - 1, ub)
    else if(value == ub + 1)
      IntervalDomain(lb, ub + 1)
    else if(lb <= value && value <= ub)
      this
    else
      super.wizz(value)
  }

  override def union(other: IntDomain): IntDomain = other match {
    case o: IntervalDomain if !emptyIntersection(other) =>
      IntervalDomain(math.min(lb, o.lb), math.max(ub, o.ub))

    case o => super.union(other)
  }

  override def equals(o: Any): Boolean = o match {
    case x: IntDomain if isEmpty && x.isEmpty => true
    case x: IntDomain if isEmpty || x.isEmpty => false
    case x: IntervalDomain                    => lb == x.lb && ub == x.ub
    case x: EnumeratedDomain =>
      lb == x.lb && ub == x.ub && size == x.size // continuous enumerated domain
    case _ => super.equals(o)
  }

  override def +(o: IntDomain): IntDomain = o match {
    case x: IntervalDomain => IntervalDomain(lb + x.lb, ub + x.ub)
    case _                 => super.+(o)
  }

  override def -(o: IntDomain): IntDomain = o match {
    case x: IntervalDomain => IntervalDomain(lb - x.ub, ub - x.lb)
    case _                 => super.-(o)
  }

  override def +(increment: Int): IntDomain = new IntervalDomain(lb + increment, ub + increment)

  override def toString =
    if(lb > ub) "{}"
    else if(lb == ub) lb.toString
    else s"[$lb, $ub]"

  override def min(other: IntDomain): IntDomain = other match {
    case x: IntervalDomain => IntervalDomain(math.min(lb, x.lb), math.min(ub, x.ub))
    case _                 => super.min(other)
  }
  override def max(other: IntDomain): IntDomain = other match {
    case x: IntervalDomain => IntervalDomain(math.max(lb, x.lb), math.max(ub, x.ub))
    case _                 => super.max(other)
  }
}

object IntervalDomain {
  def apply(lb: Int, ub: Int): IntervalDomain =
    if(lb > ub) EmptyDomain
    else new IntervalDomain(lb, ub)
}
