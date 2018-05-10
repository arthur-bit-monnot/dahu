package dahu.planning.pddl.parser

import dahu.planning.model.common.{Id, Scope, Type}
import dahu.planning.model.full.{Ctx, StaticExpr}

trait Resolver {
  def scope: Scope = ctx.scope
  def ctx: Ctx

  def predef: PddlPredef

  def typeOf(name: String): Type
  def id(name: String): Id
  def variable(name: String): StaticExpr

  def getTranslator(name: String): FunctionCompat
}

object Resolver {
  def typeOf(name: String)(implicit ctx: Resolver): Type = ctx.typeOf(name)

  def id(name: String)(implicit ctx: Resolver): Id = ctx.id(name)

}
