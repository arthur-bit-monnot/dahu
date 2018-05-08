package dahu.planning.pddl.parser

import dahu.planning.model.common._
import dahu.planning.model.full._
import Utils._
import dahu.utils.errors._
import fr.uga.pddl4j.parser.{Exp, Op}

import scala.collection.JavaConverters._

class ActionFactory(actionName: String, parent: Resolver, model: Model) extends Factory {
  implicit def predef = parent.predef
  private var template = ActionTemplate(actionName, model)

  private val start = LocalVar(resolver.id(predef.StartSym), resolver.predef.Time)
  private val end = LocalVar(resolver.id(predef.EndSym), resolver.predef.Time)

  rec(LocalVarDeclaration(start))
  rec(LocalVarDeclaration(end))

  override def context: ActionTemplate = template

  override def getTranslator(name: String): FunctionCompat = parent.getTranslator(name)

  private def duration: StaticExpr = BinaryExprTree(operators.Sub, end, start)

  def rec(block: InActionBlock): Unit = {
    template = template + block
  }

  def loadOp(op: Op): Unit = {
    require(op.getName.getImage == actionName)

    op.getParameters.asScala.foreach {
      case ast.TypedSymbol(name, tpe) =>
        rec(ArgDeclaration(Arg(resolver.id(name), resolver.typeOf(tpe))))
    }

    Option(op.getDuration) match {
      case Some(ast.Eq(ast.Duration(_), ast.Cst(cst))) =>
        rec(
          BooleanAssertion(BinaryExprTree(operators.Eq, duration, cst))
        )
      case x => unexpected(x.toString)
    }

    recPrecondition(op.getPreconditions)
    recEffect(op.getEffects)
  }

  private def recPrecondition(pre: Exp): Unit = pre match {
    case ast.And(subs) =>
      subs.foreach(recPrecondition)
    case ast.AtStart(e) =>
      e match {
        case ast.AssertionOnFunction(funcName) =>
          val assertion = resolver.getTranslator(funcName).condition(e, resolver)
          rec(
            TemporallyQualifiedAssertion(
              Equals(ClosedInterval(start, start)),
              assertion
            )
          )
      }
    case ast.AtEnd(e) =>
      e match {
        case ast.AssertionOnFunction(funcName) =>
          val assertion = resolver.getTranslator(funcName).condition(e, resolver)
          rec(
            TemporallyQualifiedAssertion(
              Equals(ClosedInterval(end, end)),
              assertion
            )
          )
      }
    case ast.OverAll(e) =>
      e match {
        case ast.AssertionOnFunction(funcName) =>
          val assertion = resolver.getTranslator(funcName).condition(e, resolver)
          rec(
            TemporallyQualifiedAssertion(
              Equals(ClosedInterval(start, end)),
              assertion
            )
          )
      }
  }

  private def recEffect(pre: Exp): Unit = pre match {
    case ast.And(subs) =>
      subs.foreach(recEffect)
    case ast.AtStart(e) =>
      e match {
        case ast.AssertionOnFunction(funcName) =>
          val assertion = resolver.getTranslator(funcName).effect(e, resolver)
          rec(
            TemporallyQualifiedAssertion(
              Equals(LeftOpenInterval(start, BinaryExprTree(operators.Add, start, predef.Epsilon))),
              assertion
            )
          )
      }
    case ast.AtEnd(e) =>
      e match {
        case ast.AssertionOnFunction(funcName) =>
          val assertion = resolver.getTranslator(funcName).effect(e, resolver)
          rec(
            TemporallyQualifiedAssertion(
              Equals(LeftOpenInterval(end, BinaryExprTree(operators.Add, end, predef.Epsilon))),
              assertion
            )
          )
      }
  }

  def result: ActionTemplate = template
}
