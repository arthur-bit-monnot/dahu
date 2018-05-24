package dahu.solvers

import dahu.model.ir.Total
import dahu.model.problem.LazyTree
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types.Value

import scala.concurrent.duration.Deadline

abstract class PartialSolver[X] {

  type K <: X

  def nextSatisfyingAssignment(deadlineMs: Option[Deadline]): Option[X => Option[Value]]
}

object PartialSolver {

  trait Builder {
    def apply[X](ast: LazyTree[X, Total, IR, _]): PartialSolver[X]
  }

}
