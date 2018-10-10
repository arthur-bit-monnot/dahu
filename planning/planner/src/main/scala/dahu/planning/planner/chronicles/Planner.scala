package dahu.planning.planner.chronicles

import cats.effect.IO
import dahu.model.input._
import dahu.planning.model.common.{Arg, Predef}
import dahu.planning.model.core
import dahu.planning.planner.PlannerConfig
import dahu.z3.Z3PartialSolver
import dahu.model.input.dsl._
import dahu.model.interpreter.Interpreter.Res
import dahu.model.problem.API
import dahu.planning.planner.hcsp.Encoder
import dahu.solvers.MetaSolver
import dahu.solvers.problem.EncodedProblem
import dahu.utils.Vec

import scala.concurrent.duration.Deadline
import dahu.utils.debug._
import dahu.utils.errors._

object Planner {

  val backend = Z3PartialSolver.builder

  def solveIncremental(model: core.CoreModel, maxSteps: Int, deadline: Deadline)(
      implicit cfg: PlannerConfig,
      predef: Predef): Option[Plan] = {
    val q = new java.util.concurrent.ConcurrentLinkedQueue[Integer]()
    for(i <- cfg.minInstances to cfg.maxInstances)
      q.add(i)

    val task: IO[Option[Plan]] = IO {
      while(deadline.hasTimeLeft) {
        val step: Integer = q.poll()
        if(step == null)
          return None

        info(s"Depth: $step")
        solveWithGivenActionNumbers(model, _ => step, deadline, symBreak = cfg.symBreak) match {
          case Some(sol) =>
            info(s"  Solution found at depth $step")
            return Some(sol)
          case None =>
            info(s"  No solution at depth $step")
        }
      }
      None
    }

    task.unsafeRunTimed(deadline.timeLeft).flatten
  }

  def solveWithGivenActionNumbers(model: core.CoreModel,
                                  num: core.ActionTemplate => Int,
                                  deadline: Deadline,
                                  symBreak: Boolean)(implicit predef: Predef): Option[Plan] = {

    if(deadline.isOverdue())
      return None

    val pb = Encoder.encode(model, num, symBreak)

    //        println(result)
    val solution = Planner.solve(pb, deadline)
    //        println(solution)
    solution
  }

  def solve(pb: EncodedProblem[Solution], deadline: Deadline): Option[Plan] = {
    if(deadline.isOverdue)
      return None

    info("  Encoding...")
    val solver = MetaSolver.of(pb, backend)

    solver.nextSolution(Some(deadline)) match {
      case Some(ass) =>
        ass.eval(pb.res) match {
          case dahu.model.interpreter.FEval(sol) =>
            val plan = Plan(sol.operators, sol.effects)
            Some(plan)
          case _ => unexpected
        }
      case None => None
    }
  }

}
