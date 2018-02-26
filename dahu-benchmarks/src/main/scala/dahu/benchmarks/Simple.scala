package dahu.benchmarks

import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.problem.SatisfactionProblem

object Simple extends Family("simple") {
  var counter = 0
  def variable() =
    Input[Int]({ counter += 1; "v" + counter.toString }).subjectTo(x => x >= 0 && x <= 10)

  val v1 = variable()
  val v2 = variable().subjectTo(_ >= v1)
//  val v3 = variable().subjectTo(_ >= v2)

  val sat = v1 === 1
  val view = Cst("OK").subjectTo(_ => sat)

  println(SatisfactionProblem.encode(view, dahu.model.compiler.Algebras.coalgebra))

  instance("base")(SatProblem(sat, NumSolutions.Exactly(1)))

}
