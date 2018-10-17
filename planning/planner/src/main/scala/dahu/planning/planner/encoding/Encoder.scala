package dahu.planning.planner.encoding

import dahu.model.input._
import dahu.model.math.any
import dahu.planning.model.common.Predef
import dahu.planning.model.core
import dahu.planning.planner.{encoding, PlannerConfig, SymBreakLevel}
import dahu.solvers.problem.{EncodedProblem, Struct}
import dahu.utils.Vec

object Encoder {
  private val PB_NAME = "__problem__"

  def encode(model: core.CoreModel, num: core.ActionTemplate => Int, exactDepth: Option[Int])(
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
      forall[CondTok](
        cond =>
          exists[EffTok](
            eff =>
              cond.fluent ==== eff.fluent &&
                cond.value ==== eff.value &&
                IntervalF.contains(eff.persistenceInterval, cond.interval) &&
                cond.supportingAction === eff.container &&
                cond.decLvl >= eff.insLvl
        )))

    if(cfg.useExactDepth) {
      exactDepth match {
        case Some(d) =>
          csp.addConstraint(
            exists[Operator](op => op.depth === Cst(d))
          )
      }
    }

    cfg.symBreak match {
      case SymBreakLevel.No =>
      case SymBreakLevel.Base =>
        csp.addConstraint(
          forall[Operator](op =>
            op.depth === Cst(1) || op.name ==== Cst(PB_NAME) || exists[Operator](op2 =>
              any
                .EQ(op.name, op2.name) && op.depth === (op2.depth + Cst(1)) && op2.start >= op.start))
        )
      case SymBreakLevel.PlanSpace | SymBreakLevel.PlanSpaceUnconstrained =>
        csp.addConstraint(
          forall[Operator](op =>
            op.depth === Cst(1) || op.name ==== Cst(PB_NAME) || exists[Operator](op2 =>
              (op.name ==== op2.name) && op.depth === (op2.depth + Cst(1)) && op.insLvl > op2.insLvl))
        )
        csp.addConstraint(
          forall[Operator](op =>
            (op.name ==== Cst(PB_NAME)) || exists[CondTok](c =>
              op.id === c.supportingAction && c.decLvl === op.insLvl))
        )
        csp.addConstraint(forall[Operator](op =>
          forall[CondTok](cond =>
            (cond.supportingAction === op.id) ==> (op.insLvl <= cond.decLvl))))

        csp.addConstraint(forall[Operator](o1 =>
          forall[Operator](o2 => (o1.insLvl < o2.insLvl) ==> (o1.lastDecLvl < o2.firstDecLvl))))

        if(cfg.symBreak == SymBreakLevel.PlanSpaceUnconstrained) {
          val numConditions = collect[CondTok].size
          val numActions = collect[Operator].size
          csp.addConstraint(
            forallUnconditional[Operator](
              op =>
                forallUnconditional[Operator](op2 =>
                  ((op.name ==== op2.name) && op.depth > op2.depth) ==>
                    ((op.insLvl > op2.insLvl) && (op.firstDecLvl > op2.lastDecLvl))))
          )

          csp.addConstraint(forallUnconditional[Operator](o1 =>
            forallUnconditional[Operator](o2 =>
              (o1.insLvl > o2.insLvl) ==> (o1.firstDecLvl > o2.lastDecLvl))))

          csp.addConstraint(
            forallUnconditional[Operator](e => e.insLvl < e.firstDecLvl)
          )

          csp.addConstraint(
            forallUnconditional[CondTok](
              e =>
                (Cst(1) <= e.decLvl) &&
                  (e.decLvl <= numConditions) &&
                  (Cst(0) <= e.supportingAction) &&
                  (e.supportingAction < numActions))
          )
          csp.addConstraint(
            forallUnconditional[Operator](
              e =>
                (Cst(1) <= e.firstDecLvl) &&
                  (e.lastDecLvl <= numConditions) &&
                  (Cst(0) <= e.insLvl) &&
                  (e.insLvl <= numConditions))
          )
        }
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

    // TODO: export a more general chronicle rather than an operator
    val dummyOp: Expr[Operator] =
      csp.asOperator(PB_NAME, Seq(), csp.temporalOrigin, csp.temporalHorizon, 0)
    csp.addExport(dummyOp)

    val flat = csp.flattened
    val actions: Expr[Vec[Operator]] = all[Operator]
    val effects: Expr[Vec[EffTok]] = all[EffTok]
    val conditions: Expr[Vec[CondTok]] = all[CondTok]

    Struct.encode(flat,
                  Product(encoding.SolutionF[Expr](actions, effects, conditions))(SolutionF.tag))

  }

}
