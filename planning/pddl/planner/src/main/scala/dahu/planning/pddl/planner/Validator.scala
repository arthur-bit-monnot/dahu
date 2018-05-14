package dahu.planning.pddl.planner

import java.io.File

import ammonite.ops._
import dahu.planning.pddl.parser.PddlPredef

import dahu.utils.debug._
import scala.util.{Failure, Success, Try}

object Validator {

  def validate(domain: File, problem: File, plan: PddlPlan)(
      implicit predef: PddlPredef): Boolean = {
    val tolerance: Double = 1.0 / predef.discretization.toDouble
    val out = File.createTempFile("plan", "")
    write.over(Path(out), plan.format)
    Try(
      %%('validate,
         "-v",
         "-t",
         tolerance,
         domain.getAbsolutePath,
         problem.getAbsolutePath,
         out.getAbsolutePath)(pwd)) match {
      case Success(_) =>
        info("Plan validated.")
      case Failure(e: ShelloutException) =>
        error("Invalid plan:")
        println(e)
    }

    true
  }

}
