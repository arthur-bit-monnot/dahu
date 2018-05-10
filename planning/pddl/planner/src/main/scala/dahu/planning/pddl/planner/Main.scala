package dahu.planning.pddl.planner

import java.io.File

import dahu.planning.pddl.parser._
import dahu.planning.planner._
import dahu.utils.Vec

import scala.concurrent.duration._
import scala.util.{Failure, Success}

case class Config(problemFile: File = null,
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

    arg[File]("XXX.dom.pddl").optional().action {
      case (f, c) => c.copy(domainFile = Some(f))
    }

    arg[File]("XXXX.YY.pb.pddl").action {
      case (f, cfg)      => cfg.copy(problemFile = f)
    }
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
      solve(dom, pb, cfg)
    case None => sys.exit(1)
  }

  type Plan = String
  def solve(domain: File, problem: File, config: Config): Option[Plan] = {
    val pddlOptions = Options(discretization = config.discretization)
    val parser = new Parser(pddlOptions)

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
            println("Validating")
            Validator.validate(domain, problem, sol)
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
