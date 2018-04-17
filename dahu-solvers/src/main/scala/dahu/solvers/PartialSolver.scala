package dahu.solvers

import dahu.constraints.interval.Interval
import dahu.model.ir.{Total, TotalSubAST}
import dahu.model.problem.SatisfactionProblemFAST.RootedLazyTree
import dahu.model.types.Value
import dahu.solvers.problem.IntCSP

abstract class PartialSolver[X](final val ast: RootedLazyTree[X, Total, cats.Id]) {

  type K <: X

  def nextSatisfyingAssignment(deadlineMs: Long = -1): Option[X => Option[Value]]
}

object PartialSolver {

  trait Builder {
    def apply[X](ast: RootedLazyTree[X, Total, cats.Id]): PartialSolver[X]
  }

}
