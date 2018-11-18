package dahu.solvers.solution
import cats.Id
import dahu.graphs.ASG
import dahu.model.input.Expr
import dahu.model.interpreter.{Interpreter, PEval}
import dahu.model.ir.Total

trait Solution {

  def eval[T](expr: Expr[T]): PEval[T]

}

sealed trait SolverResult
case object Timeout extends SolverResult
case object Unsat extends SolverResult
case class Sol(res: ASG[Expr[Any], Total, Id], next: () => SolverResult)
    extends SolverResult
    with Solution {

  lazy val cata = res.fixID.cata(Interpreter.evalAlgebra)

  def eval[T](expr: Expr[T]): PEval[T] =
    cata.get(expr).asInstanceOf[PEval[T]]

}
//case class
