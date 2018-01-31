package dahu.constraints.domains

import dahu.utils.debug._

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

  def intersection(other: IntDomain): IntDomain = slow {
    val intersection = this.values.toSet.filter(i => other.contains(i))
    new EnumeratedDomain(intersection)
  }

  def &(other: IntDomain): IntDomain = intersection(other)

  def union(other: IntDomain): IntDomain = slow {
    val union = this.values.toSet ++ other.values
    new EnumeratedDomain(union)
  }

  def min(other: IntDomain): IntDomain = slow {
    IntDomain(values.filter(_ <= other.ub).toSet ++ other.values.filter(_ <= ub).toSet)
  }
  def max(other: IntDomain): IntDomain = slow {
    IntDomain(values.filter(_ >= other.lb).toSet ++ other.values.filter(_ >= lb).toSet)
  }

  def U(other: IntDomain): IntDomain = union(other)

  def emptyIntersection(other: IntDomain): Boolean = (this & other).size == 0

  def containedBy(other: IntDomain): Boolean = (this & other).size == this.size

  final def contains(other: IntDomain): Boolean = other.containedBy(this)

  def isSingleton: Boolean = size == 1

  def head: Int = { assert1(nonEmpty); values.head }

  def isEmpty: Boolean = size == 0

  def nonEmpty: Boolean = !isEmpty

  def without(toRm: IntDomain): IntDomain =
    slow { new EnumeratedDomain(values.toSet.filterNot(i => toRm.contains(i))) }

  final def \(toRm: IntDomain): IntDomain = without(toRm)

  def without(toRm: Int): IntDomain =
    slow { new EnumeratedDomain(values.toSet - toRm) }

  final def \(toRm: Int): IntDomain = without(toRm)

  def wizz(value: Int): IntDomain =
    slow { new EnumeratedDomain(values.toSet + value) }

  def +(o: IntDomain): IntDomain =
    slow { new EnumeratedDomain((for(i <- values; j <- o.values) yield i + j).toSet) }

  def -(o: IntDomain): IntDomain =
    slow { new EnumeratedDomain((for(i <- values; j <- o.values) yield i - j).toSet) }

  def +(o: Int): IntDomain =
    slow { new EnumeratedDomain(values.map(_ + o).toSet) }

  def -(o: Int): IntDomain = this.+(-o)

  final def U(value: Int): IntDomain = wizz(value)

  override def equals(o: Any): Boolean = o match {
    case o: IntDomain => slow { o.values.toSet == values.toSet }
    case _         => false
  }

  override def hashCode(): Int = lb + ub * 31 + size * 31 * 31

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

//  def apply(values: Int*): IntDomain = IntDomain(values.toSet)
}
