package dahu.planning.pddl.parser.optim

import dahu.planning.model.common._
import dahu.planning.model.core._
import dahu.utils.errors._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class InvariantInference(original: CoreModel)(implicit predef: Predef) {

  private val staticAssertions = mutable.HashMap[FluentTemplate, ArrayBuffer[TimedAssertion]]()
  private val possibleAssertions = mutable.Map[FluentTemplate, ArrayBuffer[TimedAssertion]]()
  private def statics(f: FluentTemplate): ArrayBuffer[TimedAssertion] =
    staticAssertions.getOrElseUpdate(f, ArrayBuffer())
  private def dynamics(f: FluentTemplate): ArrayBuffer[TimedAssertion] =
    possibleAssertions.getOrElseUpdate(f, ArrayBuffer())

  original.foreach {
    case e: TimedAssertion =>
      statics(e.fluent.template) += e
    case ActionTemplate(_, blocks) =>
      blocks.foreach {
        case e: TimedAssertion =>
          dynamics(e.fluent.template) += e
        case _ =>
      }
    case _ =>
  }

  def staticConditions(fluent: FluentTemplate): Set[Expr] =
    statics(fluent).collect {
      case TimedTransitionAssertion(_, _, from, _) => from
      case TimedEqualAssertion(_, _, v)            => v
    }.toSet
  def dynamicConditions(fluent: FluentTemplate): Set[Expr] =
    dynamics(fluent).collect {
      case TimedTransitionAssertion(_, _, from, _) => from
      case TimedEqualAssertion(_, _, v)            => v
    }.toSet
  def staticEffects(fluent: FluentTemplate): Set[Expr] =
    statics(fluent).collect {
      case TimedTransitionAssertion(_, _, _, to) => to
      case TimedAssignmentAssertion(_, _, v)     => v
    }.toSet
  def dynamicEffects(fluent: FluentTemplate): Set[Expr] =
    dynamics(fluent).collect {
      case TimedTransitionAssertion(_, _, _, to) => to
      case TimedAssignmentAssertion(_, _, v)     => v
    }.toSet
  def fluents: Set[FluentTemplate] = staticAssertions.keySet.toSet ++ possibleAssertions.keySet

//  for(f <- fluents if dynamicEffects(f).isEmpty) {
//    println(f)
//    println("  cond stat: " + staticConditions(f))
//    println("  cond dyna: " + dynamicConditions(f))
//    println("   eff stat: " + staticEffects(f))
//    println("   eff dyna: " + dynamicEffects(f))
//  }
//  println("")
  def allConds(f: FluentTemplate): Set[Expr] = dynamicConditions(f) ++ staticConditions(f)
  def allEffects(f: FluentTemplate): Set[Expr] = dynamicEffects(f) ++ staticEffects(f)

  def rewritten: CoreModel = getRewriter.apply(original)
  def getRewriter: CoreModel => CoreModel =
    (in: CoreModel) => fluents.foldLeft(in)((m, f) => getRewriter(f)(m))
  private def getRewriter(fluent: FluentTemplate): CoreModel => CoreModel = {
    if(dynamicEffects(fluent).isEmpty) {
      val cst = ConstantTemplate(fluent.id, fluent.typ, fluent.params)
      (in: CoreModel) =>
        {
          in.map {
            case FunctionDeclaration(f) if f == fluent => FunctionDeclaration(cst)
            case TimedAssignmentAssertion(_, f, v) if f.template == fluent =>
              StaticAssignmentAssertion(
                new BoundConstant(cst, f.params.map(_.asInstanceOf[Cst])),
                v.asInstanceOf[Cst]
              )
            case e: TimedAssertion if e.fluent.template == fluent => unexpected
            case act @ ActionTemplate(id, content) =>
              val nc = content.map {
                case TimedEqualAssertion(_, f, v) if f.template == fluent =>
                  StaticBooleanAssertion(Op2(operators.Eq, v, Constant(cst, f.params)))
                case e: TimedAssertion if e.fluent.template == fluent => unexpected
                case x                                                => x
              }
              ActionTemplate(id, nc)
            case x => x
          }
        }
    } else {
      identity
    }
  }
}
