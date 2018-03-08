package dahu.model.input

sealed trait Ident

object Ident {

  implicit object ordering extends Ordering[Ident] {
    override def compare(x: Ident, y: Ident): Int = (x, y) match {
      case (Named(s1), Named(s2))         => Ordering[String].compare(s1, s2)
      case (_: Named, _: Anonymous)       => -1
      case (_: Anonymous, _: Named)       => 1
      case (a1: Anonymous, a2: Anonymous) => Ordering[Int].compare(a1.id, a2.id)
    }
  }
}

final case class Named(name: String) extends Ident {
  require(!name.startsWith("?"), "Reserved for printing anonymous instances.")

  override def toString: String = name
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
