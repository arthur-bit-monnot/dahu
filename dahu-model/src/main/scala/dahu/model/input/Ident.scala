package dahu.model.input

sealed trait Ident

final case class Named(name: String) extends Ident {
  require(!(name.startsWith("?") && name.endsWith("?")),
          "Reserved for printing anonymous instances.")

  override def toString: String = name
}

sealed trait Anonymous extends Ident {

  /** Only used for printing, equality relies on object identity. */
  protected val id: Int

  override def toString: String = s"?$id?"
}

object Anonymous {
  private var counter = 0
  private def next(): Int = { counter += 1; counter - 1 }

  def apply(): Anonymous = new Anonymous {
    override final val id: Int = next()
  }
}
