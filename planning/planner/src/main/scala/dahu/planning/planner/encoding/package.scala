package dahu.planning.planner
import dahu.model.functions.->:
import dahu.model.input.Expr
import dahu.model.types.{Bool, Tag}
import dahu.planning.model.common.{FluentTemplate, FunctionTemplate}

package object encoding {

  type Interval = IntervalF[cats.Id]
  type Fluent = FluentF[cats.Id]
  type CondTok = CondTokF[cats.Id]
  val CondTok: Tag[CondTok] = CondTokF.productTag

  type ContCondTok = ContCondTokF[cats.Id]
  val ContCondTok: Tag[ContCondTok] = ContCondTokF.productTag

  type SCondTok = SCondTokF[cats.Id]
  type EffTok = EffTokF[cats.Id]
  val EffTok: Tag[EffTok] = EffTokF.productTag

  type SEffTok = SEffTokF[cats.Id]

  type Solution = SolutionF[cats.Id]

  type Operator = OperatorF[cats.Id]

  import dahu.model.input.dsl._

  implicit class IntervalOps(val itv: Expr[Interval]) extends AnyVal {
    def start: Expr[Int] = IntervalF.Start(itv)
  }

  implicit class FluentFOps(val fluent: Expr[Fluent]) extends AnyVal {
    def template: Expr[FunctionTemplate] = FluentF.Template(fluent)
  }

  implicit class OperatorOps(val op: Expr[Operator]) extends AnyVal {
    def start: Expr[Int] = OperatorF.Start(op)
    def name: Expr[String] = OperatorF.Name(op)
    def depth: Expr[Int] = OperatorF.Depth(op)
    def insLvl: Expr[Int] = OperatorF.InsLvl(op)
    def id: Expr[Int] = OperatorF.Id(op)
    def firstDecLvl: Expr[Int] = OperatorF.FirstDecLvl(op)
    def lastDecLvl: Expr[Int] = OperatorF.LastDecLvl(op)
  }

  implicit class CondTokOps(val c: Expr[CondTok]) extends AnyVal {
    def fluent: Expr[Fluent] = CondTokF.Fluent(c)
    def predicate: Expr[Literal ->: Bool] = CondTokF.Predicate(c)
    def interval: Expr[Interval] = CondTokF.Itv(c)
    def decLvl: Expr[Int] = CondTokF.DecLvl(c)
    def insLvl: Expr[Int] = CondTokF.InsLvl(c)
    def supportingAction: Expr[Int] = CondTokF.SupportingAction(c)

  }

  implicit class ContCondTokOps(val c: Expr[ContCondTok]) extends AnyVal {
    def interval: Expr[Interval] = ContCondTokF.Itv(c)
    def predicate: Expr[Bool] = ContCondTokF.Predicate(c)

  }

  implicit class EffTokOps(val c: Expr[EffTok]) extends AnyVal {
    def fluent: Expr[Fluent] = EffTokF.Fluent(c)
    def value: Expr[Literal] = EffTokF.Value(c)
    def persistenceInterval: Expr[Interval] = EffTokF.Persistence(c)
    def insLvl: Expr[Int] = EffTokF.InsLvl(c)
    def container: Expr[Int] = EffTokF.Container(c)
  }

  implicit class SolutionOps(val c: Expr[Solution]) extends AnyVal {
    def continuousConditions = SolutionF.ContConds(c)
  }

}
