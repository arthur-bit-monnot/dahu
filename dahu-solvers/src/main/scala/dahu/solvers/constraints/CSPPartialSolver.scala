package dahu.solvers.constraints

import dahu.constraints.interval.Interval
import dahu.model.ir.TotalSubAST
import dahu.model.problem.IntBoolSatisfactionProblem
import dahu.model.types.TagIsoInt
import dahu.solvers.PartialSolver
import dahu.solvers.problem.IntCSP
import dahu.utils.errors._

class CSPPartialSolver[AST <: TotalSubAST[_]](_ast: AST) extends PartialSolver[AST](_ast) {

  val intBoolPb = new IntBoolSatisfactionProblem[ast.type](ast)

  private val intPB = IntCSP.intProblem(intBoolPb)
  private val csp = intPB.getSolver

  override def nextSatisfyingAssignment(deadlineMs: Long = -1): Option[ast.PartialAssignment] = {
    if(deadlineMs != -1)
      dahu.utils.debug.warning("CSP Partial solver does not support deadlines yet.")
    csp.nextSolution() match {
      case Some(ass) =>
        val partial: ast.PartialAssignment = (k: ast.VID) => {
          ass
            .get(k)
            .map(v =>
              ast.tree(k).typ match {
                case t: TagIsoInt[_] => t.toValue(v)
                case _               => unexpected
            })
        }
        Some(partial)
      case None => None
    }
  }
}

object CSPPartialSolver {

  object builder extends PartialSolver.Builder {
    override def apply(ast: TotalSubAST[_]): CSPPartialSolver[ast.type] =
      new CSPPartialSolver(ast)
  }
}
