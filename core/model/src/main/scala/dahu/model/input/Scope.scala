package dahu.model.input
import dahu.model.input.Scope.{RootScope, SubContext}
import dahu.model.math.bool
import dahu.model.types.Bool

sealed trait Scope {
  def id: String
  def present: Expr[Bool]
  def isRoot: Boolean = RootScope == this

  def subId(localId: String): String = id match {
    case "" => localId
    case x  => x + "." + localId
  }

  def subScope(name: String, presence: Expr[Bool]): Scope = SubContext(name, this, presence)

  override def toString: String = id

}

object Scope {
  def root: Scope = RootScope

  private final case object RootScope extends Scope {
    override def id: String = ""
    override def present: Expr[Bool] = bool.True
  }

  private final case class SubContext(name: String, parent: Scope, override val present: Expr[Bool])
      extends Scope {
    override val id: String = parent.subId(name)
  }

}
