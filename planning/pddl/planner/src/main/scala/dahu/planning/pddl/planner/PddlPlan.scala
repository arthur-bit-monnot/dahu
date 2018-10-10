package dahu.planning.pddl.planner

import dahu.planning.pddl.parser.PddlPredef
import dahu.utils.Vec
import cats.implicits._
import dahu.planning.planner.encoding.Plan

case class PddlPlan(operators: Vec[PddlOperator]) {

  def format: String =
    operators
      .sortedBy(_.start)
      .mkString("\n")
}

object PddlPlan {

  def apply(genPlan: Plan)(implicit predef: PddlPredef): PddlPlan = {
    PddlPlan(
      genPlan.operators
        .map(PddlOperator(_))
        .sortedBy(_.start))
  }
}
