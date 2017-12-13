package dahu.interpreter.domains

abstract class Domain[T] {

  type ValueType

  def top: T
  def bottom: T
}

trait DomainAndOr[T] {

  def and(a: T, b: T): T
  def or(a: T, b: T): T

  /** Laws
  * and(a, b) == and(b, a)
  * and(a, a) == a
  * or(a, a) == a
  * and(a, top) == a
  * and(
  */
}

sealed abstract class BoolDomain(canBeTrue: Boolean, canBeFalse: Boolean)

object BoolDomain {
  case object Top    extends BoolDomain(true, true)
  case object Bottom extends BoolDomain(false, false)
  case object True   extends BoolDomain(canBeTrue = true, canBeFalse = false)
  case object False  extends BoolDomain(canBeTrue = false, canBeFalse = true)

  implicit val typeclass = new Domain[BoolDomain] with DomainAndOr[BoolDomain] {
    override type ValueType = Boolean

    override def top: BoolDomain    = Top
    override def bottom: BoolDomain = Bottom

    override def and(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
      case (Top, x)         => x
      case (x, Top)         => x
      case (x, y) if x == y => x
      case _                => Bottom
    }

    override def or(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
      case (Bottom, x)      => x
      case (x, Bottom)      => x
      case (x, y) if x == y => x
      case _                => Top

    }
  }
}
