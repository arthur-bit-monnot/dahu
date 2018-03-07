package dahu.benchmarks

import dahu.graphs.DAG
import dahu.model.input._

case class SatProblem(private val formula: Tentative[Boolean], numSolutions: NumSolutions) {
  lazy val pb = {
    val dag = DAG[cats.Id, Tentative[Any]]
    val inputs = dag.descendantsAndSelf(formula).collect { case x: Input[_] => x }
    val inputMap = inputs.toList.map(in => in.name -> in).toMap
    SubjectTo(Product.fromMap(inputMap), formula)
  }
  def this(formula: Tentative[Boolean], numSolutions: Int) =
    this(formula, NumSolutions.Exactly(numSolutions))
}

sealed trait NumSolutions

object NumSolutions {
  case object Unknown extends NumSolutions
  case class Exactly(num: Int) extends NumSolutions
  case class AtLeast(num: Int) extends NumSolutions
}
