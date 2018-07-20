package dahu.solvers

import dahu.model.ir.Total
import dahu.model.problem.{IDTop, LazyTree}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types.Value

import scala.concurrent.duration.Deadline

abstract class PartialSolver[X, AstID] {

  type K <: X

  def nextSatisfyingAssignment(deadlineMs: Option[Deadline]): Option[X => Option[Value]]
  def nextSatisfyingAssignmentInternal(deadlineMs: Option[Deadline]): Option[AstID => Option[Value]]
}

object PartialSolver {

  trait Builder {
    def apply[X, AstID <: IDTop](ast: LazyTree[X, Total, IR, AstID]): PartialSolver[X, AstID]
  }

}
