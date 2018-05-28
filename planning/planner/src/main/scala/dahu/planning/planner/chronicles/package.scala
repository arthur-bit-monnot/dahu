package dahu.planning.planner

import dahu.model.input.Expr
import dahu.model.input.dsl._

package object chronicles {

  type Interval = IntervalF[cats.Id]
  type Fluent = FluentF[cats.Id]
  type CondTok = CondTokF[cats.Id]
  type SCondTok = SCondTokF[cats.Id]
  type EffTok = EffTokF[cats.Id]
  type SEffTok = SEffTokF[cats.Id]
  type Chronicle = ChronicleF[cats.Id]

  type Action = ActionF[cats.Id]

  implicit class ActionFOps(private val lhs: Expr[Action]) extends AnyVal {
    def start: Expr[Int] = ActionF.Start(lhs)
    def end: Expr[Int] = ActionF.End(lhs)
  }

  type Operator = OperatorF[cats.Id]
}
