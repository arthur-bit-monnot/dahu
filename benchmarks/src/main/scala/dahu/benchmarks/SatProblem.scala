package dahu.benchmarks

import dahu.graphs.DAG
import dahu.model.input._

case class SatProblem(pb: Expr[_], numSolutions: NumSolutions) {
  def this(formula: Expr[Boolean], numSolutions: Int) =
    this(formula, NumSolutions.Exactly(numSolutions))
}
object SatProblem {

  def fromSat(formula: Expr[Boolean], numSolutions: NumSolutions): SatProblem = {
    val dag = DAG[cats.Id, Expr[Any]]
    val inputs = dag.descendantsAndSelf(formula).collect { case x @ Input(Ident.Provided(_)) => x }
    val inputMap = inputs.toList.map(in => in.id -> in).toMap
    val pb = SubjectTo(Product.fromMap(inputMap), formula)
    new SatProblem(pb, numSolutions)
  }

  def fromSat(formula: Expr[Boolean], numSolutions: Int): SatProblem =
    fromSat(formula, NumSolutions.Exactly(numSolutions))

  def fromExpr[T](expr: Expr[T], numSolutions: NumSolutions) =
    new SatProblem(expr, numSolutions)

}

sealed trait NumSolutions

object NumSolutions {
  case object Unknown extends NumSolutions
  case class Exactly(num: Int) extends NumSolutions
  case class AtLeast(num: Int) extends NumSolutions
}
