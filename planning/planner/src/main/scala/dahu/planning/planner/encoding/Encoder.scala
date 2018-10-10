package dahu.planning.planner.encoding

import dahu.model.input._
import dahu.model.math.any
import dahu.planning.model.common.Predef
import dahu.planning.model.core
import dahu.planning.planner.{encoding, PlannerConfig, SymBreakLevel}
import dahu.solvers.problem.{EncodedProblem, Struct}
import dahu.utils.Vec
import dahu.utils.debug._
import dahu.utils.errors._

object Encoder {

  def encode(model: core.CoreModel, num: core.ActionTemplate => Int)(
      implicit predef: Predef,
      cfg: PlannerConfig): EncodedProblem[Solution] = {
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

    cfg.symBreak match {
      case SymBreakLevel.No =>
      case SymBreakLevel.Base =>
        csp.addConstraint(
          forall[Operator](op =>
            op.depth === Cst(1) || exists[Operator](op2 =>
              any
                .EQ(op.name, op2.name) && op.depth === (op2.depth + Cst(1)) && op2.start >= op.start))
        )
    }

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
    Struct.encode(flat, Product(encoding.SolutionF[Expr](actions, effects))(SolutionF.tag))

  }

}
