package dahu.constraints.domains

import dahu.utils.assertions._

trait IntDomain {

  /** Number of values in the domain */
  def size: Int

  def contains(value: Int): Boolean

  /** All values in the domain */
  def values: Iterable[Int]

  /** Lowest value in the domain */
  def lb: Int

  /** Highest value in the domain */
  def ub: Int

  def intersection(other: IntDomain): IntDomain = {
    val intersection = this.values.toSet.filter(other.contains)
    new EnumeratedDomain(intersection)
  }

  def &(other: IntDomain): IntDomain = intersection(other)

  def union(other: IntDomain): IntDomain = {
    val union = this.values.toSet ++ other.values
    new EnumeratedDomain(union)
  }

  def +(other: IntDomain): IntDomain = union(other)

  def emptyIntersection(other: IntDomain) = (this & other).size == 0

  def containedBy(other: IntDomain) = (this & other).size == this.size

  def isSingleton: Boolean = size == 1

  def head: Int = { assert1(nonEmpty); values.head }

  def isEmpty: Boolean = size <= 0

  def nonEmpty = !isEmpty

  def remove(toRm: IntDomain): IntDomain =
    new EnumeratedDomain(values.toSet.filterNot(toRm.contains))

  def -(toRm: IntDomain) = remove(toRm)

  def remove(toRm: Int): IntDomain =
    new EnumeratedDomain(values.toSet - toRm)

  def -(toRm: Int): IntDomain = remove(toRm)

  def --(toRemove: Iterable[Int]): IntDomain =
    toRemove.foldLeft(this)((dom, toRm) => dom - toRm)

  def add(value: Int): IntDomain =
    new EnumeratedDomain(values.toSet + value)

  def +(value: Int): IntDomain = add(value)

  override def equals(o: Any): Boolean = o match {
    case o: IntDomain => o.values.toSet == values.toSet
    case _         => false
  }

  override def toString: String =
    if (isSingleton)
      values.head.toString
    else if (size <= 6)
      s"{${values.mkString(", ")}}"
    else
      s"{${values.take(3).mkString(", ")}, ..., ${values.takeRight(3)}}"
}

object IntDomain {

  /** Factory for Domain, using the most adapted representation for the given values */
  def apply(values: Set[Int]): IntDomain =
    if (values.isEmpty) EmptyDomain
    else if (values.size == 1) SingletonDomain(values.head)
    else new EnumeratedDomain(values)

  def apply(values: Int*): IntDomain = IntDomain(values.toSet)
}
