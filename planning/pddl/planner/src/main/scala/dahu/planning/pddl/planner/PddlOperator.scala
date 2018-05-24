package dahu.planning.pddl.planner

import dahu.planning.pddl.parser.PddlPredef
import dahu.planning.planner.chronicles._
import dahu.utils.Vec

case class PddlOperator(name: String, args: Vec[String], start: Double, duration: Double) {
  override def toString: String = s"$start: ($name ${args.mkString(", ")}) [$duration]"
}

object PddlOperator {

  def toPddlTime(t: Int)(implicit predef: PddlPredef): Double =
    t.toDouble / predef.discretization.toDouble

  def apply(gen: Operator)(implicit predef: PddlPredef): PddlOperator = gen match {
    case OperatorF(name, args, start, end) =>
      new PddlOperator(name, args.map(_.toString), toPddlTime(start), toPddlTime(end - start))
  }
}
