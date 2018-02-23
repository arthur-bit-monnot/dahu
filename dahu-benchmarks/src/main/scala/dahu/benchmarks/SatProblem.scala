package dahu.benchmarks

import dahu.model.input._

case class SatProblem(formula: Tentative[Boolean], numSolutions: NumSolutions) {
  def this(formula: Tentative[Boolean], numSolutions: Int) =
    this(formula, NumSolutions.Exactly(numSolutions))
}

sealed trait NumSolutions

object NumSolutions {
  case object Unknown extends NumSolutions
  case class Exactly(num: Int) extends NumSolutions
  case class AtLeast(num: Int) extends NumSolutions
}
