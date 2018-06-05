package dahu.planning.pddl.planner

import java.io.File

import ammonite.ops._
import dahu.planning.pddl.parser.PddlPredef

import dahu.utils.debug._
import scala.util.{Failure, Success, Try}

object Validator {

  def validate(domainString: String, problemString: String, plan: PddlPlan)(
      implicit predef: PddlPredef): Boolean = {
    val tolerance: Double = 1.0 / predef.discretization.toDouble
    val domain = File.createTempFile("domain", "")
    write.over(Path(domain), domainString)
    val problem = File.createTempFile("problem", "")
    write.over(Path(problem), problemString)
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
        true
      case Failure(e: ShelloutException) =>
        error("Validation failed:")
        println(e)
        false
      case Failure(e) => throw e
    }
  }

}
