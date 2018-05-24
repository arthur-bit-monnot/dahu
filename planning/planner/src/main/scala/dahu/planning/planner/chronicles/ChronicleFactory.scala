package dahu.planning.planner.chronicles

import dahu.model.input.dsl._
import dahu.model.input.{Sequence, Expr => E}
import dahu.planning.model.common._
import dahu.planning.model.{common, core}
import dahu.model.math.bool
import dahu.planning.planner.{Literal, ProblemContext}

case class ChronicleFactory(ctx: ProblemContext,
                            conditions: List[E[CondTok]],
                            effects: List[E[EffTok]],
                            constraints: List[E[Boolean]],
                            actions: List[E[Option[Action]]]) {

  import ctx._

  def encode(orig: core.Fluent)(implicit argRewrite: Arg => E[Literal]): E[Fluent] =
    FluentF.ofExpr(orig.template, orig.params.map(ctx.encode(_)))

  def encode(orig: common.Constant)(implicit argRewrite: Arg => E[Literal]): E[Fluent] =
    FluentF.ofExpr(orig.template, orig.params.map(ctx.encode(_)))

  def encode(orig: common.Interval[Expr])(implicit argRewrite: Arg => E[Literal]): IntervalF[E] =
    orig.map(ctx.encodeAsInt(_)) match {
      case ClosedInterval(s, e)    => IntervalF.ofExprUnwrapped(s, e)
      case LeftOpenInterval(s, e)  => IntervalF.ofExprUnwrapped(s - (-1), e)
      case RightOpenInterval(s, e) => IntervalF.ofExprUnwrapped(s, e - 1)
      case OpenInterval(s, e)      => IntervalF.ofExprUnwrapped(s - (-1), e - 1)
    }

  def extended(e: core.InActionBlock)(implicit argRewrite: Arg => E[Literal]): ChronicleFactory =
    e match {
      case _: core.LocalVarDeclaration => this
      case _: core.ArgDeclaration      => this
//      case BindAssertion(c, v) => //TODO
//        val cond = ConditionToken(
//          itv = ClosedInterval(ctx.temporalOrigin, ctx.temporalHorizon),
//          fluent = encode(c),
//          value = encode(v)
//        )
//        copy(
//          conditions = cond :: conditions
//        )

      case core.TimedAssignmentAssertion(itv, fluent, value) =>
        val changeItv = encode(itv)
        val persistenceEnd = anonymousTp().subjectTo(changeItv.end <= _)
        val token =
          EffTokF.ofExpr(changeItv.start, changeItv.end, None, encode(fluent), ctx.encode(value))
//        val token = EffectToken(
//          changeItv,
//          persistenceEnd = persistenceEnd,
//          fluent = encode(fluent),
//          value = encode(value)
//        )
        copy(effects = token :: effects)

      case core.TimedEqualAssertion(itv, f, v) =>
        val interval = encode(itv)
        val token = CondTokF.ofExpr(interval.start, interval.end, encode(f), ctx.encode(v))
//        val token = ConditionToken(
//          itv = itv.map(encodeAsInt),
//          fluent = encode(f),
//          value = encode(v)
//        )
        copy(conditions = token :: conditions)

      case core.TimedTransitionAssertion(ClosedInterval(s, e), f, v1, v2) =>
        val start = encodeAsInt(s)
//        val changeEnd = encodeAsInt(e)
//        val persistenceEnd = anonymousTp().subjectTo(changeEnd <= _)
        val cond = //ConditionToken(ClosedInterval(start, start), encode(f), encode(v1))
          CondTokF.ofExpr(start, start, encode(f), ctx.encode(v1))
        val changeItv = encode(LeftOpenInterval(s, e))
        val eff = EffTokF.ofExpr(changeItv.start, changeItv.end, None, encode(f), ctx.encode(v2))
//        val eff =
//          EffectToken(LeftOpenInterval(start, changeEnd), persistenceEnd, encode(f), encode(v2))
        copy(
          conditions = cond :: conditions,
          effects = eff :: effects
        )

      case core.StaticAssignmentAssertion(lhs, rhs) =>
        val eff = EffTokF.ofExpr(ctx.temporalOrigin,
                                 ctx.temporalOrigin,
                                 Some(ctx.temporalHorizon),
                                 encode(lhs),
                                 ctx.encode(rhs))
//        val eff = EffectToken(
//          changeItv = ClosedInterval(ctx.temporalOrigin, ctx.temporalOrigin),
//          persistenceEnd = ctx.temporalHorizon,
//          fluent = encode(lhs),
//          value = encode(rhs)
//        )
        copy(
          effects = eff :: effects
        )
      case core.StaticBooleanAssertion(e) =>
        val c = ctx.boolUnbox(ctx.encode(e))
        copy(
          constraints = c :: constraints
        )
    }
  import dahu.model.input.dsl._
  def compile: E[Chronicle] = {
    ChronicleF.ofExpr(bool.And(constraints: _*),
                      Sequence(conditions),
                      Sequence(effects),
                      Sequence(actions))
  }

//  private def sameFluent(f1: Fluent, f2: Fluent): Tentative[Boolean] = {
//    if(f1.template != f2.template)
//      bool.False
//    else {
//      assert(f1.args.size == f2.args.size)
//      val identical = f1.args.zip(f2.args).map { case (p1, p2) => ctx.eqv(p1, p2) }
//      and(identical: _*)
//    }
//  }

//  def toSatProblem = {
//    var count = 0
//    val acts: Seq[Opt[Action[Tentative]]] = actions
//    val effs: Seq[Opt[EffectToken]] =
//      effects.map(Opt.present) ++
//        acts.flatMap(oa => oa.a.chronicle.effects.map(Opt(_, oa.present)))
//    val conds: Seq[Opt[ConditionToken]] =
//      conditions.map(Opt.present) ++
//        acts.flatMap(oa => oa.a.chronicle.conditions.map(Opt(_, oa.present)))
//    val consts =
//      constraints ++
//        acts.map(a => a.present.implies(and(a.a.chronicle.constraints: _*)))
//
//    val nonOverlappingEffectsConstraints =
//      for(e1 <- effs; e2 <- effs if e1 != e2) yield {
//        count += 1
//        (e1, e2) match {
//          case (Opt(EffectToken(changeItv1, end1, fluent1, _), p1),
//                Opt(EffectToken(changeItv2, end2, fluent2, _), p2)) =>
//            implies(and(p1, p2, sameFluent(fluent1, fluent2)),
//                    Interval.point(end1) < changeItv2 || Interval.point(end2) < changeItv1)
//        }
//      }
//
//    val supportConstraints =
//      conds.map {
//        case Opt(ConditionToken(persItv, fc, vc), pc) =>
//          val disjuncts = effs.map {
//            case Opt(EffectToken(changeItv, persistenceEnd, fe, ve), pe) =>
//              and(pe,
//                  sameFluent(fc, fe),
//                  ctx.eqv(vc, ve),
//                  changeItv <= persItv,
//                  persItv <= Interval.point(persistenceEnd))
//          }
//          implies(pc, or(disjuncts: _*))
//      }
//
//    val allConstraints = consts ++ nonOverlappingEffectsConstraints ++ supportConstraints
//
//    val tmp = and(allConstraints: _*)
//
//    val view = acts.map {
//      case Opt(a, present) =>
//        Product(
//          Operator[Tentative](Cst(a.name)(Tag.default),
//                              Product.fromSeq(a.args),
//                              a.start,
//                              a.end,
//                              present))
//    }
//
//    SubjectTo(Product.fromSeq(view), and(allConstraints: _*))
//  }
}

object ChronicleFactory {
  def empty(ctx: ProblemContext): ChronicleFactory =
    new ChronicleFactory(ctx = ctx,
                         conditions = Nil,
                         effects = Nil,
                         constraints = Nil,
                         actions = Nil)
}
