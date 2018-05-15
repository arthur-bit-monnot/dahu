package dahu.model.input

sealed trait Ident

object Ident {

  def apply(ref: AnyRef): Ident = Provided(ref)
  def anonymous(): Ident = Anonymous.apply()

  implicit object ordering extends Ordering[Ident] {
    override def compare(x: Ident, y: Ident): Int = (x, y) match {
      case (Provided(s1), Provided(s2))   => Ordering[String].compare(s1.toString, s2.toString)
      case (_: Provided, _: Anonymous)    => -1
      case (_: Anonymous, _: Provided)    => 1
      case (a1: Anonymous, a2: Anonymous) => Ordering[Int].compare(a1.id, a2.id)
    }
  }
}

final case class Provided(ref: AnyRef) extends Ident {
  require(ref match {
    case name: String => !name.startsWith("?")
    case _            => true
  }, "String id reserved for printing anonymous instances.")

  override def toString: String = ref.toString
}

sealed trait Anonymous extends Ident {

  /** Only used for printing, equality relies on object identity. */
  protected[input] val id: Int

  override def toString: String = "?" + id
}

object Anonymous {
  private var counter = 0
  private def next(): Int = { counter += 1; counter - 1 }

  def apply(): Anonymous = new Anonymous {
    override final val id: Int = next()
  }
}
