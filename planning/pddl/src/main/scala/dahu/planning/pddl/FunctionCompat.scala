package dahu.planning.pddl

import dahu.planning.model.common
import dahu.planning.model.common._
import dahu.planning.model.full._
import dahu.planning.pddl.Utils._
import dahu.planning.pddl.Resolver._

import dahu.utils.errors._
import fr.uga.pddl4j.parser._

import scala.collection.JavaConverters._

abstract class FunctionCompat() {
  def name: String
  def model: FluentTemplate

  def condition(e: Exp): TimedEqualAssertion // TODO: should probably take a context
  def effect(e: Exp): TimedAssignmentAssertion
}

object FunctionCompat {
  def apply(pddl: NamedTypedList)(implicit ctx: Resolver): FunctionCompat = {
    pddl.getTypes.asScala match {
      case Seq()    => new DefaultPredicate(pddl)
      case Seq(tpe) => new DefaultFunction(pddl)
      case _        => unexpected
    }
  }
}

class DefaultPredicate(pddl: NamedTypedList)(implicit ctx: Resolver) extends FunctionCompat {
  override val name: String = pddl.getName.getImage
  private val tpe = pddl.getTypes.asScala match {
    case Seq() => PddlPredef.Boolean
    case _     => unexpected
  }
  override val model =
    FluentTemplate(ctx.id(name), tpe, pddl.getArguments.asScala.map {
      case ast.TypedSymbol(argName, argType) => common.Arg(ctx.id(argName), ctx.typeOf(argType))
    })

  override def condition(e: Exp): TimedEqualAssertion = e match {
    case ast.Fluent(fun, args) if fun == name =>
      TimedEqualAssertion(
        Fluent(model, args.map(ctx.variable)),
        PddlPredef.True,
        None, //TODO
        ctx.nextId()
      )
    case _ => unexpected
  }

  override def effect(e: Exp): TimedAssignmentAssertion = e match {
    case ast.Fluent(fun, args) if fun == name =>
      TimedAssignmentAssertion(
        Fluent(model, args.map(ctx.variable)),
        predef.True,
        None, //TODO
        dahu.planning.model.reservedPrefix + next()
      )
    case _ => unexpected
  }
}

class DefaultFunction(pddl: NamedTypedList)(implicit ctx: Resolver) extends FunctionCompat {

  override val name: String = pddl.getName.getImage
  private val tpe = pddl.getTypes.asScala match {
    case Seq(t) => typeOf(t.getImage)
    case _      => unexpected
  }
  override val model =
    FluentTemplate(id(name), tpe, pddl.getArguments.asScala.map {
      case ast.TypedSymbol(argName, argType) => common.Arg(id(argName), typeOf(argType))
    })

  override def condition(e: Exp): TimedEqualAssertion = e match {
    case ast.Eq(ast.Fluent(funName, args), ast.Cst(rhs)) if funName == name =>
      TimedEqualAssertion(
        Fluent(model, args.map(ctx.variable)),
        rhs,
        None, //TODO
        dahu.planning.model.reservedPrefix + next()
      )
    case _ => unexpected
  }

  override def effect(e: Exp): TimedAssignmentAssertion = e match {
    case ast.Eq(ast.Fluent(funName, args), ast.Cst(rhs)) if funName == name =>
      TimedAssignmentAssertion(
        Fluent(model, args.map(ctx.variable)),
        rhs,
        None, //TODO
        dahu.planning.model.reservedPrefix + next()
      )
    case _ => unexpected
  }
}
