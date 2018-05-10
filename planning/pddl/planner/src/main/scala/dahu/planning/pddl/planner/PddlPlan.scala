package dahu.planning.pddl.planner

import dahu.planning.pddl.parser.PddlPredef
import dahu.planning.planner.Plan

case class PddlPlan(operators: Seq[PddlOperator]) {

  def format: String =
    operators
      .sortBy(_.start)
      .mkString("\n")
}

object PddlPlan {

  def apply(genPlan: Plan)(implicit predef: PddlPredef): PddlPlan = {
    PddlPlan(
      genPlan.operators
        .map(PddlOperator(_))
        .sortBy(_.start))
  }
}
