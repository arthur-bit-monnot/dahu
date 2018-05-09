package dahu.planning.planner

import cats.effect.IO
import cats.implicits._
import dahu.model.compiler.Algebras
import dahu.model.input.Tentative
import dahu.model.interpreter.Interpreter
import dahu.model.interpreter.Interpreter.Res
import dahu.model.types.Value
import dahu.planning.model.common.Predef
import dahu.planning.model.core
import dahu.planning.model.core.{ActionTemplate, Statement}
import dahu.solvers.MetaSolver
import dahu.utils.debug._
import dahu.utils.errors._
import dahu.z3.Z3PartialSolver

import scala.concurrent.duration._

case class PlannerConfig(minInstances: Int, maxInstances: Int, symBreak: Boolean = true)

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
    val result = model.foldLeft(Chronicle.empty(ctx)) {
      case (chronicle, statement: Statement) => chronicle.extended(statement)(_ => unexpected)
      case (chronicle, action: ActionTemplate) =>
        val actionInstances: Seq[Opt[Action[Tentative]]] =
          if(cfg.symBreak) {
            import dahu.model.input.dsl._
            (0 until step).foldLeft(List[Opt[Action[Tentative]]]()) {
              case (Nil, _) => // first action
                Opt.optional(Action.instance(action, ctx)) :: Nil
              case (last :: rest, _) =>
                // not first, enforce that this action is only present if the last one is and that its start no earliest that the last one
                val act = Opt.optional(Action.instance(action, ctx))
                val presence = act.present implies last.present
                val after = act.a.start >= last.a.start
                val withSymBreak: Opt[Action[Tentative]] = act.copy(
                  a = act.a.copy(
                    chronicle = act.a.chronicle.copy(
                      constraints = presence :: after :: act.a.chronicle.constraints
                    )))
                withSymBreak :: last :: rest
            }

          } else {
            (0 until step).map { _ =>
              Opt.optional(Action.instance(action, ctx))
            }
          }
        chronicle.copy(actions = chronicle.actions ++ actionInstances)
      case (chronicle, _) => chronicle
    }
    //        println(result)
    val solution = Planner.solve(result, deadline)
    //        println(solution)
    solution
  }

  def solve(chronicle: Chronicle, deadline: Deadline): Option[Plan] = {
    if(deadline.isOverdue)
      return None
    val sat = chronicle.toSatProblem
    info("  Encoding...")
    val solver = MetaSolver.of(Algebras.parse(sat), backend)
    val evaluatedSolution: solver.ast.Assignment => Interpreter.Result[Value] =
      (ass: solver.ast.Assignment) => {
        Interpreter.evalWithFailureCause(solver.ast)(ass)
      }
    solver.nextSolution(Some(deadline)) match {
      case Some(ass) =>
        evaluatedSolution(ass) match {
          case Res(operators) =>
            Some(
              Plan(
                operators
                  .asInstanceOf[Seq[Operator[cats.Id]]]
                  .filter(_.present)
                  .sortBy(_.start)
              ))
          case x => unexpected(x.toString)
        }
      case None => None
    }
  }

}
