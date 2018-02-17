package dahu.cerbero

object Planning extends App {

  sealed trait Ast

  final case class Sequence[+V <: Ast](seq: Seq[V]) extends Ast

  sealed trait Time                                   extends Ast
  case class Timepoint(name: String)                  extends Time
  case class RelativeTime(base: Time, delay: IntExpr) extends Time
  case class AbsoluteTime(t: Int)                     extends Time

  case class Before(left: Time, right: Time) extends BooleanExpr
  def equal(left: Time, right: Time): BooleanExpr   = and(Before(left, right), Before(right, left))
  def strictlyBefore(l: Time, r: Time): BooleanExpr = Before(RelativeTime(l, CstInt(1)), r)

  sealed trait BooleanExpr              extends Ast
  case class CstBoolean(value: Boolean) extends BooleanExpr
  case class BooleanVar(name: String)   extends BooleanExpr

  // constraint implementations

  case class And(conjuncts: Seq[BooleanExpr]) extends BooleanExpr
  case class Or(disjuncts: Seq[BooleanExpr])  extends BooleanExpr
  case class Not(e: BooleanExpr)              extends BooleanExpr
  def or(disjuncts: BooleanExpr*)                                 = Or(disjuncts)
  def ifThen(cond: BooleanExpr, effect: BooleanExpr): BooleanExpr = or(Not(cond), effect)
  def and(conjuncts: BooleanExpr*)                                = And(conjuncts)

  sealed trait IntExpr            extends Ast
  case class CstInt(value: Int)   extends IntExpr
  case class IntVar(name: String) extends IntExpr

  // constraint implementations
  case class EQ(l: IntExpr, r: IntExpr)         extends BooleanExpr
  case class LEQ(left: IntExpr, right: IntExpr) extends BooleanExpr
  case class Sum(left: IntExpr, right: IntExpr) extends IntExpr

  def nonOverlappingActions(actions: Seq[Opt[Action]]): BooleanExpr = {
    nonOverlapping(actions.map(oa => oa.map(a => a.itv)))
  }
  def nonOverlapping(intervals: Seq[Opt[Interval]]): BooleanExpr = {
    val conjuncts = for(i <- intervals.indices; j <- intervals.indices if i < j) yield {
      val a = intervals(i)
      val b = intervals(j)
      ifThen(
        and(a.present, b.present),
        or(strictlyBefore(a.value.end, b.value.start), strictlyBefore(b.value.end, a.value.start))
      )
    }
    And(conjuncts)
  }

  var i                 = 0
  def newName(): String = { i += 1; s"v$i" }

  def intVar(): IntVar       = IntVar(newName())
  def boolVar(): BooleanVar  = BooleanVar(newName())
  def timepoint(): Timepoint = Timepoint(newName())

  case class Interval(start: Time, duration: IntExpr, end: Time) extends Ast {
    def constraints: BooleanExpr =
      And(Seq(LEQ(CstInt(0), duration), Before(RelativeTime(start, duration), end)))
  }
  def interval(): Interval         = Interval(timepoint(), intVar(), timepoint())
  def interval(dur: Int): Interval = Interval(timepoint(), CstInt(dur), timepoint())

  case class Token(itv: Interval,
                   timeline: Int,
                   value: IntExpr,
                   isEffect: Boolean,
                   additionalConstraints: BooleanExpr = CstBoolean(true))
      extends Ast {
    def constraints: BooleanExpr = and(itv.constraints, additionalConstraints)
  }
  def token(): Token = Token(interval(), 0, intVar(), true)

  case class Action(itv: Interval,
                    tokens: Seq[Token],
                    additionalConstraints: BooleanExpr = CstBoolean(true))
      extends Ast {
    def constraints: BooleanExpr = And(tokens.map(_.constraints) :+ itv.constraints)
  }
  def action() = Action(interval(), Seq(token(), token()))
  case class Opt[V <: Ast](value: V, present: BooleanExpr) extends Ast {
    def subjectTo(f: V => BooleanExpr): BooleanExpr = ifThen(present, f(value))

    def map[B <: Ast](f: V => B): Opt[B] = Opt(f(value), present)
    def flatMap[B <: Ast](f: V => Opt[B]): Opt[B] =
      Opt(f(value).value, and(present, f(value).present))
  }
  object Opt {

    def optional[V <: Ast](v: V): Opt[V] = Opt(v, boolVar())
    def present[V <: Ast](v: V): Opt[V]  = Opt(v, CstBoolean(true))

    implicit class OptAction(v: Opt[Action]) {
      def interval: Opt[Interval]      = Opt[Interval](v.value.itv, v.present)
      def tokens: Opt[Sequence[Token]] = Opt(Sequence(v.value.tokens), v.present)

    }
    implicit class OptToken(v: Opt[Token]) {
      def interval: Opt[Interval] = Opt(v.value.itv, v.present)
    }
    implicit class OptInterval(v: Opt[Interval]) {
      def start: Opt[Time]       = Opt(v.value.start, v.present)
      def end: Opt[Time]         = Opt(v.value.end, v.present)
      def duration: Opt[IntExpr] = Opt(v.value.duration, v.present)
    }
    implicit class OptSeq[V <: Ast](v: Opt[Sequence[V]]) {
      def values: Seq[Opt[V]] = v.value.seq.map(x => Opt(x, v.present))
    }

  }

  case class Problem(init: Seq[Token], goals: Seq[Token], actions: Seq[Opt[Action]]) {
    lazy val tokens: Seq[Opt[Token]] = {
      init.map(Opt.present) ++
        goals.map(Opt.present) ++
        actions.flatMap(_.tokens.values)
    }
    lazy val conditionTokens: Seq[Opt[Token]] = tokens.filter(!_.value.isEffect)
    lazy val effectTokens: Seq[Opt[Token]]    = tokens.filter(_.value.isEffect)

    val satProblem: BooleanExpr = {
      val timelines = tokens.map(_.value.timeline).toSet
      val supportsByTimeline = for(i <- timelines) yield {
        val conds                 = conditionTokens.filter(_.value.timeline == i)
        val effs                  = effectTokens.filter(_.value.timeline == i)
        val nonOverlappingEffects = nonOverlapping(effs.map(_.interval))

        def support(cond: Token, optEff: Opt[Token]): BooleanExpr = {
          assert(!cond.isEffect && optEff.value.isEffect && cond.timeline == optEff.value.timeline)
          and(
            optEff.present,
            optEff.subjectTo(
              eff =>
                and(
                  Before(eff.itv.start, cond.itv.start),
                  Before(cond.itv.end, eff.itv.end),
                  EQ(cond.value, eff.value)
              ))
          )
        }

        val supportOfConditions = And(for(optCond <- conds) yield {
          optCond.subjectTo(c => Or(effs.map(e => support(c, e))))
        })
        and(supportOfConditions, nonOverlappingEffects)
      }
      val actionsConstraints = And(actions.map(_.subjectTo(_.constraints)))
      and(And(supportsByTimeline.toSeq), actionsConstraints, nonOverlappingActions(actions)) //, And(actions.map(a => Not(a.present)))) // TODO: remove
    }
  }

}
