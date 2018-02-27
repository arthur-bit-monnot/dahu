package dahu.benchmarks

import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.problem.SatisfactionProblem

object Simple extends Family("simple") {
  var counter = 0
  def variable() =
    Input[Int]({ counter += 1; "v" + counter.toString }).subjectTo(x => x >= 0 && x <= 10)

  val v1 = variable()
  val v2 = variable().subjectTo(_ > v1)

  instances("base")(
    Seq(
      SatProblem(v1 < 100, NumSolutions.Exactly(11)),
      SatProblem(v1 === 1, NumSolutions.Exactly(1)),
      SatProblem(v1 <= 1, NumSolutions.Exactly(2)),
      SatProblem(v2 === 1, NumSolutions.Exactly(1)),
      SatProblem(v2 <= 1, NumSolutions.Exactly(1)),
      SatProblem(v2 <= 2, NumSolutions.Exactly(3)),
    ))

}
