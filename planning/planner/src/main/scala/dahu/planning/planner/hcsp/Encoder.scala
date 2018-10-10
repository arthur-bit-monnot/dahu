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
    val actions: Expr[Vec[Operator]] = all[Operator]
    val effects: Expr[Vec[EffTok]] = all[EffTok]
    Struct.encode(flat, Product(SolutionF[Expr](actions, effects))(SolutionF.tag))

  }

}
