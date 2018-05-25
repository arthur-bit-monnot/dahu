package dahu.planning.planner.chronicles

import cats.effect.IO
import dahu.model.input._
import dahu.planning.model.common.Predef
import dahu.planning.model.core
import dahu.planning.planner.{PlannerConfig, ProblemContext}
import dahu.z3.Z3PartialSolver
import dahu.model.input.dsl._
import dahu.model.interpreter.Interpreter.Res
import dahu.model.problem.API
import dahu.solvers.MetaSolver

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
        solveIncrementalStep(model, step.toInt, deadline) match {
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

  def solveIncrementalStep(model: core.CoreModel, step: Int, deadline: Deadline)(
      implicit cfg: PlannerConfig,
      predef: Predef): Option[Plan] = {
    if(deadline.isOverdue())
      return None

    info("  Processing ANML model...")
    val ctx = ProblemContext.extract(model)
    val result = model.foldLeft(ChronicleFactory.empty(ctx)) {
      case (chronicle, statement: core.Statement) => chronicle.extended(statement)(_ => unexpected)
      case (chronicle, action: core.ActionTemplate) =>
        val actionInstances: Seq[Expr[Action]] =
          if(cfg.symBreak) {

            (0 until step).foldLeft(List[Expr[Action]]()) {
              case (Nil, _) => // first action
                ActionF.optionalInstance(action, ctx) :: Nil
              case (last :: rest, _) =>
                // not first, enforce that this action is only present if the last one is and that its start no earliest that the last one
                val act = ActionF.optionalInstance(action, ctx)
                val presence = (Present(act): Expr[Boolean]) implies Present(last)
                val after = act.start >= last.start
                val withSymBreak = act.subjectTo(_ => presence && after)
                withSymBreak :: last :: rest
            }

          } else {
            (0 until step).map { _ =>
              ActionF.optionalInstance(action, ctx)
            }
          }
        chronicle.copy(actions = chronicle.actions ++ actionInstances.map(_.explicitlyOptional))
      case (chronicle, _) => chronicle
    }
    //        println(result)
    val solution = Planner.solve(result.compile, deadline)
    //        println(solution)
    solution
  }

  def chroncleToPlan(c: Chronicle): Plan =
    Plan(c.actions.collect {
      case Some(a) =>
        OperatorF[cats.Id](a.name, a.args, a.start, a.end)
    })

  def solve(chronicle: Expr[Chronicle], deadline: Deadline): Option[Plan] = {
//    API.parseAndProcessPrint(chronicle)
//    return None
    chronicle
    if(deadline.isOverdue)
      return None
    val sat = chronicle
    info("  Encoding...")
    val solver = MetaSolver.of(sat, backend)
    val evaluatedSolution = solver.solutionEvaluator(sat)
    solver.nextSolution(Some(deadline)) match {
      case Some(ass) =>
        evaluatedSolution(ass) match {
          case Res(solution) =>
//            println(solution)
            Some(chroncleToPlan(solution))
          case x => unexpected(x.toString)
        }
      case None => None
    }
  }

}
