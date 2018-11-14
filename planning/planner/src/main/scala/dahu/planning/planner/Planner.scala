package dahu.planning.planner

import cats.effect.IO
import cats.implicits._
import dahu.model.ir.ProductF
import dahu.model.products.RecordType
import dahu.model.types.Tag
import dahu.planning.model.common.Predef
import dahu.planning.model.core
import dahu.planning.planner.encoding.{Encoder, Plan, Solution}
import dahu.solvers.MetaSolver
import dahu.solvers.problem.EncodedProblem
import dahu.utils._
import dahu.utils.debug.info
import dahu.utils.errors.unexpected
import dahu.z3.Z3PartialSolver

import scala.annotation.tailrec
import scala.collection.mutable
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
            extractStateTrajectory(plan)
            Some(plan)
          case x => unexpected(x.toString)
        }
      case None => None
    }
  }

  def extractStateTrajectory(p: Plan): Unit = {
    val x = p.effects
      .sortedBy(_.startChange)
      .toSeq
      .groupBy(_.fluent.template)
      .mapValues(_.map(v => (v.startChange, v.value)))
    val svs = p.effects.map(_.fluent).toSet

    println(s"State variables: $svs")

    val traj = stateOrientedView(x, Seq())(_._1, _._2)

    traj.foreach(println)
    val dstate = RecordType("dstate", "running" -> Tag.ofBoolean)

    val states = traj.map(_._2).map(s => buildState(dstate)(s)(sv => sv.id.toString))
    println(states.mkString(" \n"))
    x.foreach(println)
    sys.exit(1)
  }

  def buildState[SV, Val: ClassTag](dstate: RecordType)(s: Map[SV, Val])(
      toField: SV => String): ProductF[Val] = {
    assert(dstate.fields.size == s.size)
    assert(s.keys.map(toField).forall(dstate.fieldPosition(_).nonEmpty))
    val fields = s.toSeq
      .map {
        case (k, v) =>
          val field = toField(k)
          val fieldPos = dstate
            .fieldPosition(field)
            .getOrElse(unexpected(s"Field $field does not exists in $dstate"))
          (fieldPos, v)
      }
      .sortBy(_._1)
      .map(_._2)

    ProductF[Val](fields, dstate)
  }

  type State[SV, V] = Map[SV, V]

  @tailrec def stateOrientedView[SV, Tok, Val](timelines: Map[SV, Seq[Tok]],
                                               prevStates: Seq[(Int, State[SV, Val])])(
      time: Tok => Int,
      value: Tok => Val): Seq[(Int, State[SV, Val])] = {

    timelines.values.map(_.headOption.map(time(_)).getOrElse(Int.MaxValue)).max match {
      case Int.MaxValue =>
        prevStates
      case t =>
        val baseState: Map[SV, Val] = prevStates.lastOption.map(_._2).getOrElse(Map())
        val updates = timelines.mapValues(_.headOption.filter(time(_) == t).map(value(_))).collect {
          case (sv, Some(v)) => (sv, v)
        }
        val newState = baseState ++ updates
        println(s"$t : $updates")

        val traj = prevStates :+ ((t, newState))
        val nextTimelines = timelines.mapValues(_.filter(time(_) != t))

//        println(nextTimelines)
//        sys.exit(2)
        stateOrientedView(nextTimelines, traj)(time, value)
    }

//    val tls = mutable.Map(timelines.toSeq: _*)
//
//    for(t <- 0 until 13) {
//      println(s"\n $t")
//      val changed = tls.keys.filter(sv => tls(sv).headOption.exists(time(_) == t))
//
//      val s = changed.foldLeft(baseState)((s, sv) => s.updated(sv, value(tls(sv).head)))
//
//      println(s)
//
//      println(changed)
//
//    }

  }

}
