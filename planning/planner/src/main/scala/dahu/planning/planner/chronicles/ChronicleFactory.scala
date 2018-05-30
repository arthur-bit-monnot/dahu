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
                            staticEffects: List[E[SEffTok]],
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

  def encode(e: common.Expr)(implicit argRewrite: Arg => E[Literal]): E[Literal] =
    e match {
      case term: common.Term => ctx.encode(term)
      case Op2(operators.Eq, v, cst: common.Constant) =>
        ctx.boolBox(SCondTokF.ofExpr(encode(cst), encode(v)))
      case Op2(op, left, right) =>
        applyOperator(op, encode(left), encode(right))
    }

  def extended(e: core.InActionBlock)(implicit argRewrite: Arg => E[Literal],
                                      cnt: Counter): ChronicleFactory =
    e match {
      case _: core.LocalVarDeclaration => this
      case _: core.ArgDeclaration      => this

      case core.TimedAssignmentAssertion(itv, fluent, value) =>
        val changeItv = encode(itv)
        val persistenceEnd = anonymousTp().subjectTo(changeItv.end <= _)
        val token =
          EffTokF.ofExpr(changeItv.start, changeItv.end, None, encode(fluent), ctx.encode(value))
        copy(effects = token :: effects)

      case core.TimedEqualAssertion(itv, f, v) =>
        val interval = encode(itv)
        val token = CondTokF.ofExpr(interval.start, interval.end, encode(f), ctx.encode(v))
        copy(conditions = token :: conditions)

      case core.TimedTransitionAssertion(ClosedInterval(s, e), f, v1, v2) =>
        val start = encodeAsInt(s)
        val cond =
          CondTokF.ofExpr(start, start, encode(f), ctx.encode(v1))
        val changeItv = encode(LeftOpenInterval(s, e))
        val eff = EffTokF.ofExpr(changeItv.start, changeItv.end, None, encode(f), ctx.encode(v2))
        copy(
          conditions = cond :: conditions,
          effects = eff :: effects
        )

      case core.StaticAssignmentAssertion(lhs, rhs) =>
        val eff = SEffTokF.ofExpr(encode(lhs), ctx.encode(rhs))
        copy(
          staticEffects = eff :: staticEffects
        )
      case core.StaticBooleanAssertion(e) =>
        val c = ctx.boolUnbox(encode(e))
        copy(
          constraints = c :: constraints
        )
    }
  import dahu.model.input.dsl._
  def compile: E[Chronicle] = {
    ChronicleF.ofExpr(bool.And(constraints: _*),
                      Sequence(conditions),
                      Sequence(effects),
                      Sequence(staticEffects),
                      Sequence(actions))
  }

}

object ChronicleFactory {
  def empty(ctx: ProblemContext): ChronicleFactory =
    new ChronicleFactory(ctx = ctx,
                         conditions = Nil,
                         effects = Nil,
                         staticEffects = Nil,
                         constraints = Nil,
                         actions = Nil)
}
