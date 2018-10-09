package dahu.planning.planner.hcsp
import cats.effect.IO
import dahu.model.input._
import dahu.model.math.bool
import dahu.model.types.Tag
import dahu.planning.model.common.{Arg, LocalVar, Predef}
import dahu.planning.model.core
import dahu.planning.planner.chronicles.{
  CondTok,
  CondTokF,
  EffTok,
  EffTokF,
  IntervalF,
  Operator,
  Solution,
  SolutionF
}
import dahu.solvers.problem.{EncodedProblem, Struct}
import dahu.utils.Vec

import scala.concurrent.duration.Deadline
import dahu.utils.debug._
import dahu.utils.errors._
object Encoder {

  def encode(model: core.CoreModel, num: core.ActionTemplate => Int, symBreak: Boolean)(
      implicit predef: Predef): EncodedProblem[Solution] = {
    implicit val cnt: Counter = new Counter
//    info("  Processing ANML model...")
    val ctx = ProblemContext.extract(model)

    val csp: CSP = new RootCSP(ctx)
    implicit val resolver: VariableResolver = csp.resolver

    import dsl._

    csp.addConstraint(
      forall[EffTok](e => EffTokF.StartChange(e) <= IntervalF.Start(EffTokF.Persistence(e)))
    )

    csp.addConstraint(
      forall[EffTok](
        e1 =>
          forall[EffTok](e2 =>
            EffTokF.Id(e1) >= EffTokF.Id(e2) ||
              EffTokF.consistent(e1, e2)))
    )
    csp.addConstraint(
      forall[CondTok](cond => exists[EffTok](eff => CondTokF.supports(eff, cond)))
    )

    model.foreach {
      case core.LocalVarDeclaration(v) =>
        csp.addVar(v)
      case _ =>
    }

    model.foreach {
      case _: core.LocalVarDeclaration => // already processed
      // dealt with in ProblemContext
      case _: core.TypeDeclaration     =>
      case _: core.InstanceDeclaration =>
      case _: core.FunctionDeclaration =>
      // need action
      case statement: core.Statement   => csp.extendWith(statement)
      case action: core.ActionTemplate => csp.extendWithActions(action, num(action))
    }
    val flat = csp.flattened
    Struct.process(flat)
//    sys.exit()
    val actions: Expr[Vec[Operator]] = all[Operator]
    val effects: Expr[Vec[EffTok]] = all[EffTok]
    Struct.encode(flat, Product(SolutionF[Expr](actions, effects))(SolutionF.tag))

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
