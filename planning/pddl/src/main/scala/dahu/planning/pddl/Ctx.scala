package dahu.planning.pddl

import dahu.planning.model.common.{Id, Type}
import dahu.planning.model.full.StaticExpr

trait Ctx {
  def typeOf(name: String): Type
  def id(name: String): Id
  def variable(name: String): StaticExpr

  def nextId(): String
}

object Ctx {
  def typeOf(name: String)(implicit ctx: Ctx): Type = ctx.typeOf(name)

  def id(name: String)(implicit ctx: Ctx): Id = ctx.id(name)

}
