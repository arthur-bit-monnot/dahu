package dahu.benchmarks

import dahu.graphs.DAG
import dahu.model.input._

case class SatProblem(pb: Tentative[_], numSolutions: NumSolutions) {
  def this(formula: Tentative[Boolean], numSolutions: Int) =
    this(formula, NumSolutions.Exactly(numSolutions))
}
object SatProblem {

  def fromSat(formula: Tentative[Boolean], numSolutions: NumSolutions): SatProblem = {
    val dag = DAG[cats.Id, Tentative[Any]]
    val inputs = dag.descendantsAndSelf(formula).collect { case x: Input[_] => x }
    val inputMap = inputs.toList.map(in => in.name -> in).toMap
    val pb = SubjectTo(Product.fromMap(inputMap), formula)
    new SatProblem(pb, numSolutions)
  }

  def fromSat(formula: Tentative[Boolean], numSolutions: Int): SatProblem =
    fromSat(formula, NumSolutions.Exactly(numSolutions))

  def fromExpr[T](expr: Tentative[T], numSolutions: NumSolutions) =
    new SatProblem(expr, numSolutions)

}

sealed trait NumSolutions

object NumSolutions {
  case object Unknown extends NumSolutions
  case class Exactly(num: Int) extends NumSolutions
  case class AtLeast(num: Int) extends NumSolutions
}
