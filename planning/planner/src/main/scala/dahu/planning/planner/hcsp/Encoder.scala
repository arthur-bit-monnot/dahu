package dahu.planning.planner.hcsp
import cats.effect.IO
import dahu.model.input._
import dahu.planning.model.common.{Arg, LocalVar, Predef}
import dahu.planning.model.core

import scala.concurrent.duration.Deadline
import dahu.utils.debug._
import dahu.utils.errors._
object Encoder {

  def asChronicleExpr(model: core.CoreModel, num: core.ActionTemplate => Int, symBreak: Boolean)(
      implicit predef: Predef): CSP = {
    implicit val cnt: Counter = new Counter
//    info("  Processing ANML model...")
    val ctx = ProblemContext.extract(model)

    val csp: CSP = new RootCSP(ctx)
    implicit val resolver: VariableResolver = csp.resolver

    model.foreach {
      case core.LocalVarDeclaration(v) =>
        csp.addVar(v)
      case _ =>
    }

    model.foreach {
      case _: core.LocalVarDeclaration => // already processed
      case statement: core.Statement   => csp.extendWith(statement)
      case action: core.ActionTemplate => csp.extendWithActions(action, num(action))
    }
    csp
//    val result = model.foldLeft(ChronicleFactory.empty(ctx)) {
//      case (chronicle, statement: core.Statement) =>
//        chronicle.extended(statement)(_ => unexpected, cnt)
//      case (chronicle, action: core.ActionTemplate) =>
//        val actionInstances: Seq[Expr[Action]] =
//          if(symBreak) {
//
//            (0 until num(action)).foldLeft(List[Expr[Action]]()) {
//              case (Nil, _) => // first action
//                ActionF.optionalInstance(action, ctx) :: Nil
//              case (last :: rest, _) =>
//                // not first, enforce that this action is only present if the last one is and that its start no earliest that the last one
//                val act = ActionF.optionalInstance(action, ctx)
//                val withSymBreak = act
//                  .subjectTo(_ => Present(last))
//                  .subjectTo(_.start >= last.start)
//                withSymBreak :: last :: rest
//            }
//
//          } else {
//            (0 until num(action)).map { _ =>
//              ActionF.optionalInstance(action, ctx)
//            }
//          }
//        chronicle.copy(actions = chronicle.actions ++ actionInstances.map(_.explicitlyOptional))
//      case (chronicle, _) => chronicle
//    }
//    result.compile
  }

}
