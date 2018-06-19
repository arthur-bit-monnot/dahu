package dahu.planning.planner.chronicles

import cats.effect.IO
import dahu.model.input._
import dahu.planning.model.common.Predef
import dahu.planning.model.core
import dahu.planning.planner.{PlannerConfig, ProblemContext}
import dahu.z3.Z3PartialSolver
import dahu.model.input.dsl._
import dahu.model.interpreter.Interpreter.Res
import dahu.model.problem.{API, Group}
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

  def asChronicleExpr(model: core.CoreModel, num: core.ActionTemplate => Int, symBreak: Boolean)(
      implicit predef: Predef): Expr[Chronicle] = {
    implicit val cnt: Counter = new Counter
//    info("  Processing ANML model...")
    val ctx = ProblemContext.extract(model)
    val result = model.foldLeft(ChronicleFactory.empty(ctx)) {
      case (chronicle, statement: core.Statement) =>
        chronicle.extended(statement)(_ => unexpected, cnt)
      case (chronicle, action: core.ActionTemplate) =>
        val actionInstances: Seq[Expr[Action]] =
          if(symBreak) {

            (0 until num(action)).foldLeft(List[Expr[Action]]()) {
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
            (0 until num(action)).map { _ =>
              ActionF.optionalInstance(action, ctx)
            }
          }
        chronicle.copy(actions = chronicle.actions ++ actionInstances.map(_.explicitlyOptional))
      case (chronicle, _) => chronicle
    }
    result.compile
  }

  def solveWithGivenActionNumbers(model: core.CoreModel,
                                  num: core.ActionTemplate => Int,
                                  deadline: Deadline,
                                  symBreak: Boolean)(implicit predef: Predef): Option[Plan] = {

    if(deadline.isOverdue())
      return None

    val expr = asChronicleExpr(model, num, symBreak)
//    Group.process2(API.parse(expr))
//    System.exit(1)

    //        println(result)
    val solution = Planner.solve(expr, deadline)
    //        println(solution)
    solution
  }

  def chronicleToPlan(c: Chronicle): Plan =
    Plan(c.actions.collect {
      case Some(a) =>
        OperatorF[cats.Id](a.name, a.args, a.start, a.end)
    })

  def solve(chronicle: Expr[Chronicle], deadline: Deadline): Option[Plan] = {
    if(deadline.isOverdue)
      return None
    val sat = chronicle
    info("  Encoding...")
    val solver = MetaSolver.of(sat, backend)

    solver.nextSolution(Some(deadline)) match {
      case Some(ass) =>
        solver.solutionEvaluator(ass)(sat) match {
          case Res(solution) =>
            Some(chronicleToPlan(solution))
          case x => unexpected(x.toString)
        }
      case None => None
    }
  }

}
