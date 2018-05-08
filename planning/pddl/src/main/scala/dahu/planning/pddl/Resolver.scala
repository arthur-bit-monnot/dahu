package dahu.planning.pddl

import dahu.planning.model.common.{Id, Scope, Type}
import dahu.planning.model.full.StaticExpr

trait Resolver {
  def scope: Scope
  def typeOf(name: String): Type
  def id(name: String): Id
  def variable(name: String): StaticExpr

  def nextId(): String
}

object Resolver {
  def typeOf(name: String)(implicit ctx: Resolver): Type = ctx.typeOf(name)

  def id(name: String)(implicit ctx: Resolver): Id = ctx.id(name)

}
