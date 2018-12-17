package dahu.planning.anml.planner

import java.io.{File, FileWriter}

import cats.implicits._
import caseapp._
import caseapp.core.argparser.ArgParser
import caseapp.core.help.Help
import cats.effect.IO
import cats.implicits._
import dahu.utils.debug._
import dahu.planning.planner._
import dahu.planning.anml.parser.{Config => _, _}
import dahu.planning.planner.encoding.Plan

import scala.concurrent.duration._
import scala.concurrent.{Await, TimeoutException}
import scala.util.{Failure, Success, Try}

object ArgParsers {

  val durationParser: ArgParser[FiniteDuration] = new ArgParser[FiniteDuration] {
    override def apply(current: Option[FiniteDuration],
                       value: String): scala.Either[caseapp.core.Error, FiniteDuration] =
      Try(value.toInt) match {
        case Success(i) => Right(i.seconds)
        case Failure(_) => Left(caseapp.core.Error.UnrecognizedValue(value))
      }
    override def description: String = "duration"
  }

  val fileParser: ArgParser[File] = new ArgParser[File] {
    override def apply(current: Option[File],
                       value: String): scala.Either[caseapp.core.Error, File] =
      Right(new File(value))

    override def description: String = "duration"
  }

}

@AppName("lcp-anml")
@caseapp.ArgsName("XX.YY.pb.anml")
case class AnmlPlannerOptions(
    warmup: FiniteDuration = 0.seconds,
    timeout: FiniteDuration = 1800.seconds,
    repeat: Int = 1,
    @Recurse
    plannerOptions: PlannerConfig
)
object AnmlPlannerOptions {
  implicit val durParser = ArgParsers.durationParser
  implicit val fileParser = ArgParsers.fileParser

  val parser = caseapp.Parser[AnmlPlannerOptions]

  object instance {
    implicit val parser: Parser[AnmlPlannerOptions] = AnmlPlannerOptions.parser
  }
}

import AnmlPlannerOptions.instance._

object Main extends CaseApp[AnmlPlannerOptions] {
  private def error(msg: String): Nothing = {
    System.err.println(msg)
    sys.exit(1)
  }

  def run(cfg: AnmlPlannerOptions, arg: RemainingArgs): Unit = {
    val problemFile: File = arg.remaining match {
      case Seq(pb) => new File(pb)
      case Seq()   => error("No problem file was provided.")
      case _       => error("More than one problem file provided.")
    }

    (0 until cfg.repeat).foreach { _ =>
      implicit val cfgImpl = cfg
      dahu.utils.debug.LOG_LEVEL = 3

      if(cfg.warmup.toSeconds > 0) {
        info("Warming up...")
        dahu.utils.debug.LOG_LEVEL = 0

        solveTask(problemFile, Deadline.now + cfg.warmup)
          .map(res => Success(res))
          .unsafeRunTimed(cfg.warmup)

        dahu.utils.debug.LOG_LEVEL = 3
      }

      val startTime = System.currentTimeMillis()

      solveTask(problemFile, Deadline.now + cfg.timeout)
        .unsafeRunTimed(cfg.timeout) match {
        case Some(Some(result)) =>
          val runtime = System.currentTimeMillis() - startTime
          val fw = new FileWriter("/tmp/runtimes", true)
          val timeFmt = s"${runtime / 1000}.${(runtime % 1000) / 10}\n"
          try {
            fw.write(timeFmt)
          } finally fw.close()
          out(s"== Solution (in ${runtime / 1000}.${(runtime % 1000) / 10}s) ==")
          out(
            result.operators
              .sortedBy(_.start)
              .map {
                case dahu.planning.planner.encoding
                      .OperatorF(name, args, start, end, _, _, _, _, _) =>
                  s"[$start, $end] $name(${args.mkString(", ")})"
              }
              .mkString("\n"))
          result.continuousEvol.foreach(_.print())
        case None =>
          val fw = new FileWriter("/tmp/runtimes", true)
          try {
            fw.write("--\n")
          } finally fw.close()
          out("Time out")
        case Some(None) =>
          val fw = new FileWriter("/tmp/runtimes", true)
          try {
            fw.write("--\n")
          } finally fw.close()
          out("Max depth or time reached")
      }
    }
  }

  def solveTask(problemFile: File, deadline: Deadline)(
      implicit cfg: AnmlPlannerOptions): IO[Option[Plan]] = {
    IO {
      info(problemFile.getName)
      solve(problemFile, deadline)
    }
  }

  def solve(problemFile: File, deadline: Deadline)(
      implicit cfg: AnmlPlannerOptions): Option[Plan] = {
    implicit val plannerConf: PlannerConfig = cfg.plannerOptions
    info("Parsing...")
    parse(problemFile) match {
      case ParseSuccess(model) =>
        info("Discrete Planning")
        Planner.solveIncremental(model, deadline)
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

case class HybridPlannerOptions(
    timeout: FiniteDuration = 1800.seconds,
    @caseapp.ExtraName("c")
    contDom: File,
    @caseapp.ExtraName("d")
    discPb: File,
    @Recurse
    plannerOptions: PlannerConfig
)
object HybridPlannerOptions {
  implicit val durParser = ArgParsers.durationParser
  implicit val fileParser = ArgParsers.fileParser

  val parser = caseapp.Parser[HybridPlannerOptions]
  val help = caseapp.core.help.Help[HybridPlannerOptions]
  object instance {
    implicit val parser: Parser[HybridPlannerOptions] = HybridPlannerOptions.parser
    implicit val help: Help[HybridPlannerOptions] = HybridPlannerOptions.help
  }
}

object Hybrid
    extends CaseApp[HybridPlannerOptions]()(HybridPlannerOptions.parser, HybridPlannerOptions.help) {

  private def error(msg: String): Nothing = {
    System.err.println(msg)
    sys.exit(1)
  }

  def run(cfg: HybridPlannerOptions, arg: RemainingArgs): Unit = {
    implicit val cfgImpl = cfg
    dahu.utils.debug.LOG_LEVEL = 3

    val startTime = System.currentTimeMillis()

    solveTask(cfg.discPb, cfg.contDom, Deadline.now + cfg.timeout)
      .unsafeRunTimed(cfg.timeout) match {
      case Some(Some(result)) =>
        val runtime = System.currentTimeMillis() - startTime
        val fw = new FileWriter("/tmp/runtimes", true)
        val timeFmt = s"${runtime / 1000}.${(runtime % 1000) / 10}\n"
        try {
          fw.write(timeFmt)
        } finally fw.close()
        out(s"== Solution (in ${runtime / 1000}.${(runtime % 1000) / 10}s) ==")
        out(
          result.operators
            .sortedBy(_.start)
            .map {
              case dahu.planning.planner.encoding
                    .OperatorF(name, args, start, end, _, _, _, _, _) =>
                s"[$start, $end] $name(${args.mkString(", ")})"
            }
            .mkString("\n"))
        result.continuousEvol.foreach(_.print())
      case None =>
        val fw = new FileWriter("/tmp/runtimes", true)
        try {
          fw.write("--\n")
        } finally fw.close()
        out("Time out")
      case Some(None) =>
        val fw = new FileWriter("/tmp/runtimes", true)
        try {
          fw.write("--\n")
        } finally fw.close()
        out("Max depth or time reached")
    }

  }

  def run(discOptions: PlannerConfig, contProblem: File, discProblem: File): Unit = {}

  def solveTask(problemFile: File, continuousDomain: File, deadline: Deadline)(
      implicit cfg: HybridPlannerOptions): IO[Option[Plan]] = {
    IO {
      info(problemFile.getName)
      solve(problemFile, continuousDomain, deadline)
    }
  }

  def solve(problemFile: File, continuousDomain: File, deadline: Deadline)(
      implicit cfg: HybridPlannerOptions): Option[Plan] = {
    implicit val plannerConf: PlannerConfig = cfg.plannerOptions
    info("Parsing...")
    parse(problemFile) match {
      case ParseSuccess(model) =>
        info("Continuous Planning")
        Planner.solveIncrementalContinuous(model, continuousDomain, deadline)

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
