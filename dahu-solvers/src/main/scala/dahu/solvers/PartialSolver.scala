package dahu.solvers

import dahu.constraints.interval.Interval
import dahu.model.ir.TotalSubAST
import dahu.solvers.problem.IntCSP

abstract class PartialSolver[AST <: TotalSubAST[_]](final val ast: AST) {

  type K <: ast.ID

  def nextSatisfyingAssignment(deadlineMs: Long = -1): Option[ast.PartialAssignment]
}

object PartialSolver {

  trait Builder {
    def apply(ast: TotalSubAST[_]): PartialSolver[ast.type]
  }

}
