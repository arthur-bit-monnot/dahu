package dahu.planning.rcll

import ammonite.ops._
import caseapp._
import dahu.planning.pddl.planner.Config

import scala.concurrent.duration._

case class Options(r: Int = 1,
                   complexity: Int = 0,
                   outputDir: String = "out/",
                   game: String = "001",
                   solve: Boolean = true,
                   domain: String =
                     "/home/arthur/work/ext/ros-rcll_ros/pddl/rcll_domain_production_durations.pddl")

object Main extends CaseApp[Options] {

  override def run(options: Options, remainingArgs: RemainingArgs): Unit = {

    val gameFile = Path(
      s"/home/arthur/work/postdoc/rcll_planner/input-files/game/game-${options.game}.txt")
    val navFile = Path(
      s"/home/arthur/work/postdoc/rcll_planner/input-files/navgraph/navgraph-costs-${options.game}.csv")

    val problemName = s"${options.game}-R${options.r}-C0"

    val game: Game = Parser.game.parse(read(gameFile)).get.value
    val nav = Parser.travelTimes.parse(read(navFile)).get.value

    val order = game.orders.find(_.complexity == options.complexity).get
    val pb = Problem(order, options.r, nav)

    val domainFile = Path(options.domain)
    val pbFile = pwd / RelPath(options.outputDir) / s"rcll.$problemName.pb.anml"

    write.over(pbFile, pb.asPddl)

    dahu.planning.pddl.planner.Main.solve(domainFile.toIO, pbFile.toIO, Config())
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
