package dahu.planning.planner

import cats.effect.IO
import cats.implicits._
import dahu.planning.model.common.Predef
import dahu.planning.model.core
import dahu.planning.planner.encoding.{Encoder, Plan, Solution}
import dahu.solvers.MetaSolver
import dahu.solvers.problem.EncodedProblem
import dahu.utils.debug.info
import dahu.utils.errors.unexpected
import dahu.z3.Z3PartialSolver

import scala.concurrent.duration.Deadline

object Planner {

  val backend = Z3PartialSolver.builder

  def solveIncremental(model: core.CoreModel, deadline: Deadline)(implicit cfg: PlannerConfig,
                                                                  predef: Predef): Option[Plan] = {
    val q = new java.util.concurrent.ConcurrentLinkedQueue[Integer]()
    for(i <- cfg.minDepth to cfg.maxDepth)
      q.add(i)

    val task: IO[Option[Plan]] = IO {
      while(deadline.hasTimeLeft) {
        val step: Integer = q.poll()
        if(step == null)
          return None

        info(s"Depth: $step")
        val exactDepth = if(cfg.useExactDepth) Some(step.toInt) else None
        solveWithGivenActionNumbers(model, _ => step, exactDepth, deadline) match {
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

  def solveWithGivenActionNumbers(
      model: core.CoreModel,
      num: core.ActionTemplate => Int,
      exactDepth: Option[Int],
      deadline: Deadline)(implicit predef: Predef, cfg: PlannerConfig): Option[Plan] = {

    if(deadline.isOverdue())
      return None

    val pb = Encoder.encode(model, num, exactDepth)

    //        println(result)
    val solution = Planner.solve(pb, deadline)
    //        println(solution)
    solution
  }

  def solve(pb: EncodedProblem[Solution], deadline: Deadline)(
      implicit cfg: PlannerConfig): Option[Plan] = {
    if(deadline.isOverdue)
      return None

    info("  Encoding...")
    val solver = MetaSolver.of(pb, backend)

    if(cfg.noSolve)
      return None

    solver.nextSolution(Some(deadline)) match {
      case Some(ass) =>
        ass.eval(pb.res) match {
          case dahu.model.interpreter.FEval(sol) =>
            if(cfg.printDetailed) {
              println("Actions: ")
              sol.operators.sortedBy(_.insertionLvl).foreach(a => println(s"  $a"))
              println("Effects:")
              sol.effects.sortedBy(_.insLvl).foreach(e => println(s"  $e"))
              println("Conditions: ")
              sol.conditions.sortedBy(_.decisionLevel).foreach(c => println(s"  $c"))
              println("Continuous Conditions: ")
              sol.continuousConditions.sortedBy(_.itv.start).foreach(c => println(s"  $c"))
            }
            val plan = Plan(sol.operators, sol.effects)
            Some(plan)
          case x => unexpected(x.toString)
        }
      case None => None
    }
  }

}
