package dahu.solvers

import dahu.model.ir.Total
import dahu.graphs._
import dahu.model.types.Value

import scala.concurrent.duration.Deadline

abstract class PartialSolver[X, AstID] {

  type K <: X

  def nextSatisfyingAssignment(clauses: Option[X],
                               deadlineMs: Option[Deadline]): Option[X => Option[Value]]
  def nextSatisfyingAssignmentInternal(clauses: Option[X],
                                       deadlineMs: Option[Deadline]): Option[AstID => Option[Value]]

}

object PartialSolver {

  trait Builder {
    def apply[X, AstID <: IDTop](ast: LazyTree[X, Total, cats.Id, AstID]): PartialSolver[X, AstID]
  }

}
