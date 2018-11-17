package dahu.planning.planner

import cats.Id
import cats.effect.IO
import cats.implicits._
import dahu.graphs.ASG
import dahu.model.input.{Cst, Expr}
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.problem.API
import dahu.model.products.RecordType
import dahu.model.types.{Bool, SequenceTag, Tag, TagAny}
import dahu.planning.model.common.Type.{Integers, Reals}
import dahu.planning.model.common.{FunctionTemplate, Predef, Type}
import dahu.planning.model.core
import dahu.planning.planner.encoding.{
  EffTok,
  EffTokF,
  Encoder,
  IntLit,
  Literal,
  ObjLit,
  Plan,
  ProblemContext,
  Solution,
  SolutionF
}
import dahu.refinement.interop.{ContinuousProblem, Params, Solver}
import dahu.solvers.MetaSolver
import dahu.solvers.problem.EncodedProblem
import dahu.utils._
import dahu.utils.debug.info
import dahu.utils.errors.unexpected
import dahu.z3.Z3PartialSolver

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.Deadline
import scala.util.{Failure, Success}

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

    val (pb, ctx) = Encoder.encode(model, num, exactDepth)

    //        println(result)
    val solution = Planner.solve(pb, deadline, ctx)
    //        println(solution)
    solution
  }

  def solve(pb: EncodedProblem[Solution], deadline: Deadline, ctx: ProblemContext)(
      implicit cfg: PlannerConfig): Option[Plan] = {
    if(deadline.isOverdue)
      return None

    info("  Encoding...")
    val solver = MetaSolver.of(pb, backend)

    if(cfg.noSolve)
      return None

    solver.nextSolutionTree(Some(deadline)) match {
      case Some(t) =>
        val t2 = t.asInstanceOf[ASG[Expr[Any], ExprF, Id]]
        val x = API
          .expandLambdasThroughPartialEval(t2)
          .transform(dahu.model.transformations
            .makeOptimizer(dahu.model.transformations.Pass.allStaticPasses))
          .transform(dahu.model.transformations
            .makeOptimizer(dahu.model.transformations.Pass.allStaticPasses))
        API.echo(x.rootedAt(pb.res))
        extractStateTrajectory(pb.res, x, ctx)
        sys.exit(1)
      case None => None
    }

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

  def extractStateTrajectory(solution: Expr[Solution],
                             _tree: ASG[Expr[Any], ExprF, Id],
                             ctx: ProblemContext): Unit = {

    val tmp = _tree.fixID.extensible
    val tree = tmp.fixID
    type I = tree.ID
    import dahu.lisp.compile.Env.ops._
    val x = //dahu.lisp.compile.Env.default(tree.internalData)
      dahu.refinement.interop.sources.default(tree.internalData)
    x.setConstantValue("solution", tree.getTreeRoot(solution))
    x.defineStruct(SolutionF.tag)

    dahu.lisp.compile.defineContinuousState(x, ctx.stateTypes.cstate)
    dahu.lisp.compile.defineDiscreteState(x, ctx.stateTypes.dstate)

    val Y = x.parse("solution").get

    import dahu.model.input.dsl._
    import dahu.planning.planner.encoding.DummyImplicits._
    import dahu.planning.planner._
    val effs = SolutionF.tag.getAccessor[Vec[EffTok]]("effects").apply(solution)

    val starts = effs.map0(e => e.persistenceInterval.start)
    val fluents =
      effs.map0(e => e.fluent.template)
    val values = effs.map0(e => e.value)

    def eval[A](i: I): Option[A] = tree.internalCoalgebra(i) match {
      case CstF(v, _) => Some(v.asInstanceOf[A])
      case _          => None
    }

    def evalsShallow[A](e: Expr[Vec[A]]): Option[List[I]] = {
      tree.getExt(e) match {
        case SequenceF(members, _) =>
          Some(members.toList)
        case _ => None

      }
    }

    def evalsAll[A](e: Expr[Vec[A]]): Option[List[A]] = {
      tree.getExt(e) match {
        case SequenceF(members, _) =>
          members
            .map(i => eval[A](i))
            .toList
            .sequence
      }
    }
    def asSequence(members: Seq[I], tag: TagAny): I = {
      tree.record(SequenceF(members, SequenceTag.of(tag)))
    }

    def literal2cst(lit: Literal, tpe: Type): I = {
      val cst = (lit, tpe) match {
        case (IntLit(i), Reals) =>
          Cst[Double](i.toDouble)
        case (IntLit(i), Integers) =>
          Cst[Int](i)
        case (ObjLit(value), _) if tpe.isBoolean =>
          assert(value.typ.isBoolean)
          if(value.toString == "true") Cst[Bool](Bool.True)
          else if(value.toString == "false") Cst[Bool](Bool.False)
          else ???
        case (ObjLit(value), _) =>
          ??? // TODO: valid but commented to catch earlier errors Cst[String](value.toString)
      }
      tree.getTreeRoot(cst)
    }

    val changes = for {
      times <- evalsAll(starts)
      svs <- evalsAll(fluents)
      valsNative <- evalsAll(values)
      vals = valsNative
        .zip(svs.map(_.typ))
        .map((literal2cst _).tupled)
    } yield times.zip(vals).zip(svs)
    println(changes)

    val xx = changes match {
      case Some(l) =>
        l.groupBy(_._2)
          .mapValues(_.map(_._1))
          .mapValues(_.sortBy(_._1))
      case None => unexpected("Cant not evaluated solution competely.")
    }
    println(xx)

//    buildState(ctx.stateTypes.dstate)(xx)(_.id.name)
    val traj = stateOrientedView(xx)(_._1, _._2)
    val states = traj.map(_._2).map(s => buildState(ctx.stateTypes.dstate)(s)(sv => sv.id.toString))
    val istates = states.map(tree.record)
    val timedStates = traj.map(_._1).zip(istates)
    println(traj)
    println(states)

    val DTRAJ = "discrete-state-trajectory"
    val HAPPS = "happenings"

    val eventsProcessing = for {
      eventStarts <- evalsAll[Int](solution.continuousConditions.map0(e => e.interval.start))
      pred <- evalsShallow(solution.continuousConditions.map0(e => e.predicate))
      events = eventStarts.zip(pred).sortBy(_._1)
    } yield {
      val eventFields = events.toSeq.indices.map(i => s"_e${i}_" -> Tag.ofBoolean)
      val eventsTpe = RecordType("events", eventFields: _*)
      dahu.lisp.compile.defineEvents(x, eventsTpe)
      val emptyEvents = x.parse("NO_EVENTS").get
      val groupdedEvents =
        eventStarts.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2).toSet)

      println(eventStarts)
      println(pred)
      val evs = groupdedEvents
        .mapValues(s => {
          val fields = eventsTpe.fields.map { f =>
            if(s.contains(f.position))
              tree.getTreeRoot(Cst(Bool.True))
            else
              tree.getTreeRoot(Cst(Bool.False))
          }
          tree.record(ProductF(fields, eventsTpe))
        })
        .toList
        .sortBy(_._1)

      val happeningTimes = (evs.map(_._1) ++ timedStates.map(_._1 - 1)).distinct.sorted
      println(happeningTimes)
      def stateOf(happeningTime: Int) = {
        timedStates.indices
          .find(i => timedStates(i)._1 > happeningTime)
          .map(i => timedStates(i)._2)
          .getOrElse(dahu.utils.errors.unexpected(s"No state for happening time $happeningTime"))
      }
      def eventsOf(happeningTime: Int) = {
        evs
          .find(e => e._1 == happeningTime)
          .map(_._2)
          .getOrElse(emptyEvents)
      }

      val statesTrajList = happeningTimes.dropRight(1).map(stateOf)
      val happeningList = happeningTimes.map(eventsOf)

      val stateTraj = asSequence(statesTrajList, ctx.stateTypes.dstate)
      val happenings = asSequence(happeningList, eventsTpe)

      def constraintOf(ev: Int): I = {
        x.parse(s"_e${ev}_")
          .map(isE => {
            val notE = tree.record(ComputationF(bool.Not, isE))
            val implies = tree.record(ComputationF(bool.Or, notE, pred(ev)))
            implies
          })
          .getOrElse(unexpected)
      }
      val dynConstraints = asSequence(pred.indices.map(constraintOf), Tag.ofBoolean)
      x.setConstantValue("dyn-constraints", dynConstraints)
      x.setConstantValue(HAPPS, happenings)
      x.setConstantValue(DTRAJ, stateTraj)
    }

    assert(eventsProcessing.nonEmpty)

    x.parseFile("domains/car/car.dom.clj") match {
      case Success(value)     =>
      case Failure(exception) => throw exception
    }
//    println(tree.build(Y))
//
//    val stateTraj = asSequence(states.map(tree.record(_)), ctx.stateTypes.dstate)
//
//
//
//
//
//    println(tree.build(stateTraj))
//    println(tree.build(eventTraj))

    val process = """
(defstruct band
  ^events start-events
  ^dstate dstate
  ^events end-events
  )

(define all-constraints (seq.concat constraints dyn-constraints))


(define bands (map (fn [i] (band
              (seq.get happenings i)
              (seq.get discrete-state-trajectory i)
              (seq.get happenings (i+ i 1i))))
     (seq.indices discrete-state-trajectory)))

(define cfun
  (meta.as-lambda current-events
                  (meta.as-lambda current-discrete-state all-constraints))
  )

(define band-constraints
  (map
   (fn [b] (list
            (cfun (band.start-events b) (band.dstate b))
            (cfun NO_EVENTS (band.dstate b))
            (cfun (band.end-events b) (band.dstate b))
            ))
   bands))
    """

    val simplified = tree.internalView

    val problem = new ContinuousProblem[I](simplified, ctx.stateTypes.cstate)

    val res = for {
      _ <- x.parseMany(process)
      bandConstraints <- x.parse("band-constraints")
      stateTraj <- x.parse(DTRAJ)
      pb <- problem.createProblem(stateTraj, bandConstraints)
    } yield {

      println(tree.build(bandConstraints))
      val solver = new Solver(pb, Params())
      solver.solve()
    }

    res match {
      case Success(value) =>
      case Failure(e)     => throw e
    }

//    API.echo(tree.rootedAt(starts))
//    API.echo(tree.rootedAt(fluents))
//    API.echo(tree.rootedAt(values))

    sys.exit(10)

//    tree.getExt(e) match {
//      case ProductF(ms, tpe) =>
//        val effList = ms(tpe.fieldPosition("effects").get)
//        tree.internalCoalgebra(effList) match {
//          case SequenceF(es, p) =>
//            assert(p.memberTag == EffTokF.productTag)
//        }
//        println(tpe)
//        println(ms.map(i => tree.internalCoalgebra(i)))
//
//        ???
//      case x => unexpected(s"$x")
//    }
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
                                               prevStates: Seq[(Int, State[SV, Val])] = Seq())(
      time: Tok => Int,
      value: Tok => Val): Seq[(Int, State[SV, Val])] = {

    timelines.values.map(_.headOption.map(time(_)).getOrElse(Int.MinValue)).max match {
      case Int.MinValue =>
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
