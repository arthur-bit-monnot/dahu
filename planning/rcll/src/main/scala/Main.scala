package dahu.planning.rcll

import java.io.File

import cats.implicits._
import ammonite.ops._
import caseapp._
import dahu.planning.pddl.parser.PddlPredef
import dahu.planning.pddl.planner.{Config, PddlPlan}
import dahu.planning.planner.PlannerConfig
import dahu.planning.planner.chronicles.{Plan, Planner}

import scala.concurrent.duration._
import scala.util.Success

case class Options(r: Int = 1,
                   complexity: Int = 0,
                   outputDir: String = "out/",
                   game: String = "001",
                   solve: Boolean = true,
                   specialized: Boolean = true,
                   domain: String =
                     "/home/arthur/work/dahu/planning/rcll/rcll_domain_production_durations.pddl")

object Main extends CaseApp[Options] {
  val pddlOptions = dahu.planning.pddl.parser.Options(discretization = 1000)
  val pddlParser = new dahu.planning.pddl.parser.Parser()(pddlOptions)
  implicit val predef: PddlPredef = pddlParser.predef

  override def run(options: Options, remainingArgs: RemainingArgs): Unit = {

    val gameFile = Path(
      s"/home/arthur/work/postdoc/rcll_planner/input-files/game/game-${options.game}.txt")
    val navFile = Path(
      s"/home/arthur/work/postdoc/rcll_planner/input-files/navgraph/navgraph-costs-${options.game}.csv")

    val problemName = s"${options.game}-R${options.r}-C${options.complexity}"

    val game: Game = Parser.game.parse(read(gameFile)).get.value
    val nav = Parser.travelTimes.parse(read(navFile)).get.value

    val order = game.orders.find(_.complexity == options.complexity).get
    val pb = Problem(order, options.r, nav)

    val domainFile = Path(options.domain)
    val pbFile = pwd / RelPath(options.outputDir) / s"rcll.$problemName.pb.anml"

    write.over(pbFile, pb.asPddl)

    val result =
      if(options.specialized)
        solveSpecialized(domainFile.toIO, pbFile.toIO, pb)
      else
        solveGeneric(domainFile.toIO, pbFile.toIO)

    result match {
      case Some(plan) => println(plan.format)
      case None       => println("OUPSIE")
    }
  }

  def solveGeneric(domain: File, problem: File): Option[PddlPlan] = {
    dahu.planning.pddl.planner.Main
      .solve(domain, problem, Config())
      .map(p => PddlPlan(p))
  }

  def solveSpecialized(domain: File, problem: File, c: Problem): Option[PddlPlan] = {
    pddlParser.parse(domain, problem) match {
      case Success(model) =>
        Planner
          .solveWithGivenActionNumbers(model,
                                       a => c.maxActions(a.name),
                                       Deadline.now + 1800.seconds,
                                       symBreak = true)
          .map(PddlPlan(_))
      case x =>
        None
    }
  }
}

object Benchmark extends App {
  val outFile = pwd / "results.txt"
  write.over(outFile, "")

  for(game <- 1 to 10; r <- 1 to 3; c <- 0 to 0) {
    val options = Options(r = r, complexity = c, game = f"$game%03d")

    val startTime = System.currentTimeMillis()
    Main.run(options, RemainingArgs(Nil, Nil))
    val endTime = System.currentTimeMillis()
    val runtime = (endTime - startTime).toFloat / 1000
    dahu.utils.debug.info("")
    println(f"\n======= $game%03d R$r C$c  ---  $runtime%1.3f ============\n")
    write.append(outFile, f"$game%03d R$r C$c    $runtime%1.3f\n")
  }
}
