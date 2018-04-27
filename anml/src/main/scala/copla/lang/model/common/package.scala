package copla.lang.model

import copla.lang.model

package object common {

  case class Id(scope: Scope, name: String) {
    override def toString: String = scope.toScopedString(name)

    def toTPId: Timepoint = new Timepoint(this)
  }

  case class Timepoint(id: Id) {
    override def toString: String = id.toString
  }

  sealed trait Scope {

    def +(nestedScope: String): InnerScope = InnerScope(this, nestedScope)

    def makeNewId(): Id = Id(this, model.defaultId())
    def toScopedString(name: String): String
  }
  object RootScope extends Scope {
    override def toString: String = "root"
    def toScopedString(name: String): String = name
  }
  case class InnerScope(parent: Scope, name: String) extends Scope {

    override def toString: String = parent match {
      case RootScope => name
      case nonScope  => s"$nonScope.$name"
    }
    def toScopedString(name: String): String = s"$this.$name"
  }

  case class Type(id: Id, parent: Option[Type]) {
    def isSubtypeOf(typ: Type): Boolean =
      this == typ || parent.exists(t => t == typ || t.isSubtypeOf(typ))

    def overlaps(typ: Type): Boolean = this.isSubtypeOf(typ) || typ.isSubtypeOf(this)

    def asScope: Scope = id.scope + id.name

    override def toString: String = id.toString
  }
  object Type {
    val Integers = Type(Id(RootScope, "integer"), None)
  }

  sealed trait Term {
    def id: Id
    def typ: Type
  }
  object Term {
    def unapply(v: Term) = Option((v.id, v.typ))
  }

  sealed trait Cst extends Term
  sealed trait Var extends Term

  case class IntLiteral(value: Int) extends Cst {
    override def typ: Type = Type.Integers
    override def id: Id = Id(RootScope + "_integers_", value.toString)
  }

  /** Variable declared locally */
  case class LocalVar(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }

  /** Instance of a given type, result of the ANML statement "instance Type id;" */
  case class Instance(id: Id, typ: Type) extends Cst {
    override def toString: String = id.toString
  }

  /** Denote the argument of the template of state variables and actions. */
  case class Arg(id: Id, typ: Type) extends Var {
    override def toString: String = id.toString
  }

  sealed trait FunctionTemplate {
    def id: Id
    def typ: Type
    def params: Seq[Arg]
  }

  case class FluentTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {
    override def toString: String = id.toString
  }

  case class ConstantTemplate(id: Id, typ: Type, params: Seq[Arg]) extends FunctionTemplate {
    override def toString: String = id.toString
  }

}
