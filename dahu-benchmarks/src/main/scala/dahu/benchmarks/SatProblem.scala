package dahu.benchmarks

import dahu.expr.Expr

case class SatProblem(formula: Expr[Boolean], numSolutions: NumSolutions) {
  def this(formula: Expr[Boolean], numSolutions: Int) = this(formula, Exactly(numSolutions))
}

sealed trait NumSolutions
case object Unknown          extends NumSolutions
case class Exactly(num: Int) extends NumSolutions
case class AtLeast(num: Int) extends NumSolutions
