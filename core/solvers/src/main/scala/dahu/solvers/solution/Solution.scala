package dahu.solvers.solution
import dahu.model.input.Expr
import dahu.model.interpreter.PEval

trait Solution {

  def eval[T](expr: Expr[T]): PEval[T]

}
