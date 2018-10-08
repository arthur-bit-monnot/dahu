package dahu.planning.anml.planner

import java.io.File

import cats.effect.IO
import dahu.utils.debug._
import dahu.planning.planner._
import dahu.planning.anml.parser.{Config => _, _}
import dahu.planning.planner.chronicles.Plan

import scala.concurrent.duration._
import scala.concurrent.{Await, TimeoutException}
import scala.util.{Failure, Success, Try}

case class Config(problemFile: File = null,
                  minInstances: Int = 0,
                  maxInstances: Int = 500,
                  symBreak: Boolean = true,
                  useXorForSupport: Boolean = true,
                  numThreads: Int = 1,
                  maxRuntime: FiniteDuration = 1800.seconds,
                  warmupTimeSec: FiniteDuration = 0.seconds)

object Main extends App {

  val optionsParser = new scopt.OptionParser[Config]("dahu") {
    head("dahu", "0.x")

    //    opt[Int]("num-threads")
    //      .action((n, c) => c.copy(numThreads = n))

    opt[Int]("warmup")
      .action((t, c) => c.copy(warmupTimeSec = t.seconds))

    opt[Int]("min-depth")
      .action((d, c) => c.copy(minInstances = d))

    opt[Int]("max-depth")
      .action((d, c) => c.copy(maxInstances = d))

    opt[Int]("timeout")
      .action((t, c) => c.copy(maxRuntime = t.seconds))

    //    opt[Boolean]("use-xor")
    //      .action((b, c) => c.copy(useXorForSupport = b))
    //
    //    opt[Boolean]("sym-break")
    //      .action((b, c) => c.copy(symBreak = b))

    arg[File]("XXXX.pb.anml").action((f, c) => c.copy(problemFile = f))
  }

  optionsParser.parse(args, Config()) match {
    case Some(cfg) =>
      implicit val cfgImpl = cfg

      if(cfg.warmupTimeSec.toSeconds > 0) {
        info("Warming up...")
        dahu.utils.debug.LOG_LEVEL = 0

        solveTask(cfg.problemFile, Deadline.now + cfg.warmupTimeSec)
          .map(res => Success(res))
          .unsafeRunTimed(cfg.warmupTimeSec)

        dahu.utils.debug.LOG_LEVEL = 3
      }

      val startTime = System.currentTimeMillis()

      solveTask(cfg.problemFile, Deadline.now + cfg.maxRuntime)
        .unsafeRunTimed(cfg.maxRuntime) match {
        case Some(Some(result)) =>
          val runtime = System.currentTimeMillis() - startTime
          out(s"== Solution (in ${runtime / 1000}.${(runtime % 1000) / 10}s) ==")
          out(result.toString)
        case None =>
          out("Time out")
        case Some(None) =>
          out("Max depth or time reached")
      }

      sys.exit(0)
    case None =>
  }

  def solveTask(problemFile: File, deadline: Deadline)(implicit cfg: Config): IO[Option[Plan]] = {
    IO {
      info(problemFile.getName)
      solve(problemFile, deadline)
    }
  }

  def solve(problemFile: File, deadline: Deadline)(implicit cfg: Config): Option[Plan] = {
    implicit val plannerConf: PlannerConfig =
      PlannerConfig(minInstances = cfg.minInstances, maxInstances = cfg.maxInstances)
    info("Parsing...")
    parse(problemFile) match {
      case ParseSuccess(model) =>
        dahu.planning.planner.hcsp.Encoder.asChronicleExpr(model, _ => 1, true)
//        Planner.solveIncremental(model, cfg.maxInstances, deadline)
        None //TODO
      case fail: ParseFailure =>
        println("Parsing failed:")
        println(fail.format)
        sys.exit(1)
      case ParserCrash(err, _) =>
        println(s"Parser crashed.")
        err.printStackTrace()
        sys.exit(1)
    }
  }

}
