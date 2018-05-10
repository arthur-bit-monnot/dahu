package dahu.planning.pddl.planner

import java.io.File

import ammonite.ops._
import dahu.planning.pddl.parser.PddlPredef

object Validator {

  def validate(domain: File, problem: File, plan: PddlPlan)(implicit predef: PddlPredef): Boolean = {
    val tolerance: Double = 1.0 / predef.discretization.toDouble
    val out = File.createTempFile("plan", "")
    write.over(Path(out), plan.format)
    val ret = %%('validate, "-t", tolerance, "-v", domain.getAbsolutePath, problem.getAbsolutePath, out.getAbsolutePath)(pwd)

    println(ret)
    true
  }

}
