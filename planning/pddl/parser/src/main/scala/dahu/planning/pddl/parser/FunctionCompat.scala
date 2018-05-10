package dahu.planning.pddl.parser

import dahu.planning.model.common
import dahu.planning.model.common._
import dahu.planning.model.full._
import Utils._

import dahu.utils.errors._
import fr.uga.pddl4j.parser._

import scala.collection.JavaConverters._

abstract class FunctionCompat() {
  def name: String
  def model: FluentTemplate

  def fluent(name: String, args: Seq[String], res: Resolver): Fluent
  def condition(e: Exp, res: Resolver): TimedEqualAssertion // TODO: should probably take a context
  def effect(e: Exp, res: Resolver): TimedAssignmentAssertion
}

object FunctionCompat {
  def apply(pddl: NamedTypedList)(implicit ctx: Resolver): FunctionCompat = {
    pddl.getTypes.asScala match {
      case Seq()    => new DefaultPredicate(pddl, ctx)
      case Seq(tpe) => new DefaultFunction(pddl, ctx)
      case _        => unexpected
    }
  }
}

class DefaultPredicate(pddl: NamedTypedList, top: Resolver) extends FunctionCompat {
  implicit private def predef = top.predef
  override val name: String = pddl.getName.getImage
  private val tpe = pddl.getTypes.asScala match {
    case Seq() => top.predef.Boolean
    case _     => unexpected
  }
  override val model =
    FluentTemplate(top.id(name), tpe, pddl.getArguments.asScala.map {
      case ast.TypedSymbol(argName, argType) => common.Arg(top.id(argName), top.typeOf(argType))
    })

  override def fluent(name: String, args: Seq[String], res: Resolver): Fluent =
    Fluent(model, args.map(res.variable))

  override def condition(e: Exp, local: Resolver): TimedEqualAssertion = e match {
    case ast.Fluent(fun, args) if fun == name =>
      TimedEqualAssertion(
        Fluent(model, args.map(local.variable)),
        local.predef.True,
        Some(local.ctx),
        local.scope.makeNewId()
      )
    case _ => unexpected
  }

  override def effect(e: Exp, local: Resolver): TimedAssignmentAssertion = e match {
    case ast.Fluent(fun, args) if fun == name =>
      TimedAssignmentAssertion(
        fluent(fun, args, local),
        local.predef.True,
        Some(local.ctx),
        local.scope.makeNewId()
      )
    case ast.Not(ast.Fluent(fun, args)) =>
      TimedAssignmentAssertion(
        fluent(fun, args, local),
        predef.False,
        Some(local.ctx),
        local.scope.makeNewId()
      )
    case _ => unexpected
  }
}

class DefaultFunction(pddl: NamedTypedList, top: Resolver) extends FunctionCompat {
  implicit private def predef = top.predef

  override val name: String = pddl.getName.getImage
  private val tpe = pddl.getTypes.asScala match {
    case Seq(t) => top.typeOf(t.getImage)
    case _      => unexpected
  }
  override val model =
    FluentTemplate(top.id(name), tpe, pddl.getArguments.asScala.map {
      case ast.TypedSymbol(argName, argType) => common.Arg(top.id(argName), top.typeOf(argType))
    })

  override def fluent(name: String, args: Seq[String], res: Resolver): Fluent =
    Fluent(model, args.map(res.variable))

  override def condition(e: Exp, local: Resolver): TimedEqualAssertion = e match {
    case ast.Eq(ast.Fluent(funName, args), ast.Cst(rhs)) if funName == name =>
      TimedEqualAssertion(
        fluent(funName, args, local),
        rhs,
        Some(local.ctx),
        local.scope.makeNewId()
      )
    case _ => unexpected
  }

  override def effect(e: Exp, local: Resolver): TimedAssignmentAssertion = e match {
    case ast.Eq(ast.Fluent(funName, args), ast.Cst(rhs)) if funName == name =>
      TimedAssignmentAssertion(
        fluent(funName, args, local),
        rhs,
        Some(local.ctx),
        local.scope.makeNewId()
      )
    case _ => unexpected
  }
}
