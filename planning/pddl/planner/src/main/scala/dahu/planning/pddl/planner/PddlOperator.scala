package dahu.planning.pddl.planner

import dahu.planning.pddl.parser.PddlPredef
import dahu.planning.planner.Operator
import dahu.utils.Vec

case class PddlOperator(name: String, args: Vec[String], start: Double, duration: Double) {
  override def toString: String = s"$start: ($name ${args.mkString(", ")}) [$duration]"
}

object PddlOperator {

  def toPddlTime(t: Int)(implicit predef: PddlPredef): Double = t.toDouble / predef.discretization.toDouble

  def apply(gen: Operator[cats.Id])(implicit predef: PddlPredef): PddlOperator = gen match {
    case Operator(name, args, start, end, true) =>
      new PddlOperator(
        name,
        Vec(args.map(_.toString): _*),
        toPddlTime(start),
        toPddlTime(end - start))
  }
}
