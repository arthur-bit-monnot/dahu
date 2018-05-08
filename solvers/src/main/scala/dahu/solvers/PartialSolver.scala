package dahu.solvers

import dahu.constraints.interval.Interval
import dahu.model.ir.{Total, TotalSubAST}
import dahu.model.problem.SatisfactionProblem.RootedLazyTree
import dahu.model.types.Value
import dahu.solvers.problem.IntCSP

import scala.concurrent.duration.Deadline

abstract class PartialSolver[X](final val ast: RootedLazyTree[X, Total, cats.Id]) {

  type K <: X

  def nextSatisfyingAssignment(deadlineMs: Option[Deadline]): Option[X => Option[Value]]
}

object PartialSolver {

  trait Builder {
    def apply[X](ast: RootedLazyTree[X, Total, cats.Id]): PartialSolver[X]
  }

}
