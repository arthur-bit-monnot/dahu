package dahu.constraints.domains

class EnumeratedDomain(val vals: IBitSet) extends IntDomain {

  def this(values: Iterable[Int]) = this(new IBitSet() ++ values)
  def this(values: java.util.BitSet) = this(new IBitSet(values.toLongArray))

  def toBitSet: java.util.BitSet = vals match {
    case bs: IBitSet => java.util.BitSet.valueOf(bs.elems)
    case _           => throw new RuntimeException("Unsupported conversion from non-IBitSet collection.")
  }

  override def values: Set[Int] = vals

  override def lb: Int = vals.min
  override def ub: Int = vals.max

  override def size: Int = vals.size

  override def intersection(other: IntDomain): IntDomain = {
    other match {
      case other: EnumeratedDomain =>
        val intersection = (vals, other.vals) match {
          case (v1: IBitSet, v2: IBitSet) =>
            v1 & v2 // should be significantly faster as it is just and 'and' on two bitset
          case (v1, v2) => v1 & v2
        }
        new EnumeratedDomain(intersection)
      case _ =>
        super.intersection(other)
    }
  }

  override def union(other: IntDomain): EnumeratedDomain = {
    other match {
      case other: EnumeratedDomain =>
        val union = (vals, other.vals) match {
          case (v1: IBitSet, v2: IBitSet) =>
            v1 | v2 // should be significantly faster as it is just and 'or' on two bitset
          case (v1, v2) => v1 | v2
        }
        new EnumeratedDomain(union)
    }
  }

  override def emptyIntersection(other: IntDomain): Boolean = {
    other match {
      case other: EnumeratedDomain =>
        !vals.sharesOneElement(other.vals) // optimized method
      case _ =>
        super.emptyIntersection(other)
    }
  }

  def contains(v: Int): Boolean =
    vals.contains(v)

  override def isEmpty: Boolean = vals.isEmpty

  override def nonEmpty = !isEmpty

  override def remove(toRm: IntDomain): IntDomain = {
    toRm match {
      case toRm: EnumeratedDomain =>
        new EnumeratedDomain(vals -- toRm.vals)
      case _ =>
        super.remove(toRm)
    }
  }

  override def remove(toRm: Int): EnumeratedDomain =
    new EnumeratedDomain(vals - toRm)

  override def add(value: Int): EnumeratedDomain = {
    new EnumeratedDomain(vals + value)
  }

  override def toString: String = s"{${values.map(_.toString).mkString(", ")}}"
}
