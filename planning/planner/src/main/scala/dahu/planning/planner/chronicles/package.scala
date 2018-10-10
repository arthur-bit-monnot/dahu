package dahu.planning.planner

import dahu.model.types.Tag

package object chronicles {

  type Interval = IntervalF[cats.Id]
  type Fluent = FluentF[cats.Id]
  type CondTok = CondTokF[cats.Id]
  val CondTok: Tag[CondTok] = CondTokF.productTag

  type SCondTok = SCondTokF[cats.Id]
  type EffTok = EffTokF[cats.Id]
  val EffTok: Tag[EffTok] = EffTokF.productTag

  type SEffTok = SEffTokF[cats.Id]

  type Solution = SolutionF[cats.Id]

  type Operator = OperatorF[cats.Id]
}
