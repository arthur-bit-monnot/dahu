package dahu.solvers.constraints

import dahu.constraints.interval.Interval
import dahu.model.ir.TotalSubAST
import dahu.solvers.{PartialSolver, Solver}
import dahu.solvers.problem.IntCSP

class CSPPartialSolver[AST <: TotalSubAST[_]](_ast: AST) extends PartialSolver[AST](_ast) {
  override type K = IntCSP.Key[ast.ID]

  private val intPB = IntCSP.intProblem(ast)
  private val csp: Solver[K, Int, Interval] = intPB.getSolver

  override def nextSatisfyingAssignment(): Option[ast.PartialAssignment] = {
    csp.nextSolution() match {
      case Some(ass) =>
        val sol = csp.extractSolution(ass)
        val partial: ast.PartialAssignment = (k: ast.VID) => {
          sol.get(k)
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
