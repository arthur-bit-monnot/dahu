package dahu.model.input
import dahu.model.types.{Tag, TagAny}

final case class Ident(scope: Scope, lid: LocalIdent) {
  override def toString: String =
    if(scope.isRoot) lid.toString
    else scope.toString + "." + lid.toString

  def subIdent(name: String): Ident = {
    val s = scope.subScope(lid.toString, scope.present)
    Ident(s, name)
  }
}
object Ident {
  def apply[LocalId](scope: Scope, lid: LocalId): Ident =
    new Ident(scope, LocalIdent(lid))
  def anonymous(scope: Scope): Ident = new Ident(scope, LocalIdent.anonymous())
}

final case class TypedIdent(id: Ident, typ: TagAny) {
  override def toString: String = id.toString
}

sealed trait LocalIdent

object LocalIdent {

  def apply(ref: Any): LocalIdent = Provided(ref)

  def unapply(arg: LocalIdent): Option[Any] = arg match {
    case Provided(x) => Some(x)
    case _           => None
  }
  def anonymous(): LocalIdent = Anonymous.apply()

  final case class Provided(ref: Any) extends LocalIdent {
    require(ref match {
      case name: String => !name.startsWith("?")
      case _            => true
    }, "String id reserved for printing anonymous instances.")

    override def toString: String = ref.toString
  }

  sealed trait Anonymous extends LocalIdent {

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

  implicit object ordering extends Ordering[LocalIdent] {
    override def compare(x: LocalIdent, y: LocalIdent): Int = (x, y) match {
      case (Provided(s1), Provided(s2))   => Ordering[String].compare(s1.toString, s2.toString)
      case (_: Provided, _: Anonymous)    => -1
      case (_: Anonymous, _: Provided)    => 1
      case (a1: Anonymous, a2: Anonymous) => Ordering[Int].compare(a1.id, a2.id)
    }
  }
}
