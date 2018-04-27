package dahu.planner

import copla.lang
import copla.lang.model.core.{ActionTemplate, Statement}
import dahu.utils.errors._
import java.io.{File, FileWriter}

import copla.lang.model.core
import dahu.model.input.Tentative
import monix.eval.{MVar, Task}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.{Await, Promise, TimeoutException}
import scala.util.{Failure, Success, Try}
import dahu.utils.debug._

case class Config(problemFile: File = null,
                  minInstances: Int = 0,
                  maxInstances: Int = 500,
                  symBreak: Boolean = true,
                  useXorForSupport: Boolean = true,
                  numThreads: Int = 1,
                  maxRuntime: Int = 1800,
                  warmupTimeSec: Int = 0)

object Main extends App {

  val parser = new scopt.OptionParser[Config]("dahu") {
    head("dahu", "0.x")

//    opt[Int]("num-threads")
//      .action((n, c) => c.copy(numThreads = n))

    opt[Int]("warmup")
      .action((t, c) => c.copy(warmupTimeSec = t))

    opt[Int]("min-depth")
      .action((d, c) => c.copy(minInstances = d))

    opt[Int]("max-depth")
      .action((d, c) => c.copy(maxInstances = d))

    opt[Int]("timeout")
      .action((t, c) => c.copy(maxRuntime = t))

//    opt[Boolean]("use-xor")
//      .action((b, c) => c.copy(useXorForSupport = b))
//
//    opt[Boolean]("sym-break")
//      .action((b, c) => c.copy(symBreak = b))

    arg[File]("XXXX.pb.anml").action((f, c) => c.copy(problemFile = f))
  }

  import dahu.utils.debug._

  parser.parse(args, Config()) match {
    case Some(cfg) =>
      implicit val cfgImpl = cfg

      if(cfg.warmupTimeSec > 0) {
        info("Warming up...")
        dahu.utils.debug.LOG_LEVEL = 0

        val warmUpTask =
          solveTask(cfg.problemFile, System.currentTimeMillis() + cfg.warmupTimeSec * 1000)
            .map(res => Success(res))
            .timeoutTo(cfg.warmupTimeSec.seconds, Task(Failure(new TimeoutException())))
            .runAsync

        Try(Await.result(warmUpTask, (cfg.warmupTimeSec + 10).seconds))

        dahu.utils.debug.LOG_LEVEL = 3
      }

      val startTime = System.currentTimeMillis()

      val future =
        solveTask(cfg.problemFile, System.currentTimeMillis() + cfg.maxRuntime * 1000)
          .map(res => Success(res))
          .timeoutTo(cfg.maxRuntime.seconds, Task(Failure(new TimeoutException())))
          .runAsync

      Await.result(future, (cfg.maxRuntime + 10).seconds) match {
        case Success(Some(result)) =>
          val runtime = System.currentTimeMillis() - startTime
          out(s"== Solution (in ${runtime / 1000}.${(runtime % 1000) / 10}s) ==")
          out(result.toString)
        case Success(None) =>
          out("Max depth or time reached")
        case Failure(_: TimeoutException) =>
          out("Timeout")
        case Failure(exception) =>
          out("Crash")
          exception.printStackTrace()
      }

      sys.exit(0)
    case None =>
  }

  def solveTask(problemFile: File, deadline: Long)(implicit cfg: Config): Task[Option[Any]] = {
    Task {
      info(problemFile.getName)
      solve(problemFile, deadline)
    }
  }

  def solve(problemFile: File, deadline: Long)(implicit cfg: Config): Option[String] = {
    info("Parsing...")
    lang.parse(problemFile) match {
      case lang.Success(model) =>
        solveIncremental(model, cfg.maxInstances, deadline)
      case lang.ParseError(fail) =>
        println("Parsing failed:")
        println(fail.format)
        sys.exit(1)
      case lang.Crash(msg, err) =>
        println(s"Parser crashed: $msg")
        err.foreach(_.printStackTrace())
        sys.exit(1)
    }
  }

  def solveIncremental(model: core.CoreModel, maxSteps: Int, deadline: Long)(
      implicit cfg: Config): Option[String] = {
    val q = new java.util.concurrent.ConcurrentLinkedQueue[Integer]()
    for(i <- cfg.minInstances to cfg.maxInstances)
      q.add(i)

    val workers = (0 until cfg.numThreads) map { workerID =>
      Task {
        while(System.currentTimeMillis() < deadline) {
          val step: Integer = q.poll()
          if(step == null)
            return None

          info(s"Depth: $step (worker: $workerID)")
          solveIncrementalStep(model, step.toInt, deadline) match {
            case Some(sol) =>
              info(s"  Solution found at depth $step (worker: $workerID)")
              return Some(sol)
            case None =>
              info(s"  No solution at depth $step (worker: $workerID)")
          }
        }
        None
      }: Task[Option[String]]
    }
    val future = Task.raceMany(workers).runAsync

    Try {
      Await.result(future, (deadline - System.currentTimeMillis()).millis)
    } match {
      case Success(solution) =>
        solution
      case Failure(to: TimeoutException) => None
      case Failure(e)                    => throw e
    }
  }

  def solveIncrementalStep(model: core.CoreModel, step: Int, deadline: Long)(
      implicit cfg: Config): Option[String] = {
    if(System.currentTimeMillis() >= deadline)
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

}
