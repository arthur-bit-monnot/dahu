package dahu.benchmarks

import dahu.model.input._
import dahu.model.input.dsl._

object Simple extends Family("simple") {
  var counter = 0
  def variable() =
    Input[Int]({ counter += 1; "v" + counter.toString }) //.subjectTo(x => x >= 0 && x <= 10)

  val v1 = variable()
  val v2 = variable().subjectTo(_ > v1)

  instances("base")(
    Seq(
      SatProblem.fromSat(v1 < 100, 11),
//      SatProblem.fromSat(v1 === 1, 1),
//      SatProblem.fromSat(v1 <= 1, 2),
//      SatProblem.fromSat(v2 === 1, 1),
//      SatProblem.fromSat(v2 <= 1, 1),
//      SatProblem.fromSat(v2 <= 2, 3),
    ))

}