package dahu.planning.pddl.planner

import java.io.File

import dahu.planning.model.ShowScoped
import dahu.planning.model.common.RootScope
import dahu.planning.model.core.{ActionTemplate, InModuleBlock}
import dahu.planning.pddl.parser._
import dahu.planning.planner.PlannerConfig
import dahu.planning.planner.chronicles._
import dahu.utils.Vec

import scala.concurrent.duration._
import scala.util.{Failure, Success}

sealed trait Mode
object Mode {
  case object Planner extends Mode
  case object Linter extends Mode

}
case class Config(mode: Mode = Mode.Planner,
                  problemFile: File = null,
                  validate: Boolean = true,
                  solve: Boolean = true,
                  domainFile: Option[File] = None,
                  minInstances: Int = 0,
                  maxInstances: Int = 500,
                  symBreak: Boolean = true,
                  useXorForSupport: Boolean = true,
                  numThreads: Int = 1,
                  maxRuntime: FiniteDuration = 1800.seconds,
                  warmupTimeSec: Int = 0,
                  discretization: Int = 1000)

object Main extends App {
  val optionsParser = new scopt.OptionParser[Config]("dahu") {
    head("dahu", "0.x")

    opt[Int]("min-depth")
      .action((d, c) => c.copy(minInstances = d))

    opt[Int]("max-depth")
      .action((d, c) => c.copy(maxInstances = d))

    opt[Int]("timeout")
      .action((t, c) => c.copy(maxRuntime = t.seconds))

    opt[Int]("discretization")
      .action((i, c) => c.copy(discretization = i))

    opt[Boolean]("validate")
      .action((b, c) => c.copy(validate = b))

    opt[Boolean]("solve")
      .action((b, c) => c.copy(solve = b))

    arg[File]("[XXX.dom.pddl] XXX.YY.pb.pddl").minOccurs(1).maxOccurs(2).action {
      case (f, cfg) if cfg.problemFile == null => cfg.copy(problemFile = f)
      case (f, cfg)                            => cfg.copy(domainFile = Some(cfg.problemFile), problemFile = f)
    }

    cmd("lint")
      .text("Analyzes the domain and problem for common problems and possible optimizations.")
      .action((_, cfg) => cfg.copy(mode = Mode.Linter))

  }

  optionsParser.parse(args, Config()) match {
    case Some(cfg) =>
      val pb = cfg.problemFile
      val dom = cfg.domainFile match {
        case Some(f) => f
        case None =>
          pb.getName.split('.') match {
            case Array(d, _, "pb", "pddl") => new File(pb.getParent, s"$d.dom.pddl")
            case _ =>
              System.err.println(
                "No domain file provided and problem file is not of the form XXXX.YY.pb.pddl")
              sys.exit(1)
          }
      }
      cfg.mode match {
        case Mode.Planner => solve(dom, pb, cfg)
        case Mode.Linter  => lint(dom, pb, cfg)
      }

    case None => sys.exit(1)
  }

  def lint(domain: File, problem: File, cfg: Config): Unit = {
    val pddlOptions = Options(discretization = cfg.discretization, lint = true)
    val parser = new Parser()(pddlOptions)
    implicit val predef: PddlPredef = parser.predef

    parser.parse(domain, problem) match {
      case Success(x) =>
        val printer = ShowScoped[InModuleBlock]
        implicit val scope = RootScope
//        println(x)
        x.foreach(b => println(printer.show(b)))
//        x.foreach {
//          case a @ ActionTemplate(_, content) =>
//            println()
//            println(a)
//            content.distinct.foreach(x => println("  " + x))
//          case _ =>
//        }
      case Failure(e) => throw e
    }
  }

  type Plan = String
  def solve(domain: File, problem: File, config: Config): Option[Plan] = {
    val pddlOptions = Options(discretization = config.discretization)
    val parser = new Parser()(pddlOptions)

    implicit val plannerConfig: PlannerConfig =
      PlannerConfig(config.minInstances, config.maxInstances)
    implicit val predef: PddlPredef = parser.predef

    parser.parse(domain, problem) match {
      case Success(model) =>
        Planner.solveIncremental(model, config.maxInstances, Deadline.now + config.maxRuntime) match {
          case Some(plan) =>
            println("\n== Solution ==")
            val sol = PddlPlan(plan)
            println(sol.format)
            if(config.validate) {
              println("Validating")
              Validator.validate(domain, problem, sol)
            }
          case None =>
            println("\nFAIL")
        }
      case Failure(err) =>
        err.printStackTrace()
        sys.exit(1)
    }
    None
  }
}
