package dahu.benchmarks

import caseapp._
import dahu.planning.pddl.planner.{Config, Main}
import dahu.planning.pddl.problems.{Extractor, PddlProblem}

import scala.concurrent.duration._

case class PddlBenchmarksOptions(runtime: Int = 60, verbose: Int = 2)

case class Run(dom: String, pb: String, solved: Boolean, runtime: Duration) {
  def format: String =
    s"$dom.$pb\t${if(solved) "OK" else "__"}\t ${PddlBenchmarks.format(runtime)}"
}

object PddlBenchmarks extends CaseApp[PddlBenchmarksOptions] {

  private def fail(): Unit = dahu.utils.errors.unexpected
  def format(d: Duration): String =
    "%.2f".formatLocal(java.util.Locale.US, d.toMillis.toFloat / 1000)

  val defaults = Seq(
    "tests" -> "blocks_01",
    "blocks_ipc2" -> "p04-0",
    "blocks_ipc2" -> "p04-1",
    "blocks_ipc2" -> "p05-0",
    "blocks_ipc2" -> "p05-1",
    "rovers_ipc5" -> "p01",
    "rovers_ipc5" -> "p02",
    "rovers_ipc5" -> "p03",
    "rcll" -> "001-R1-C0",
    "rcll" -> "001-R2-C0",
    "rcll" -> "002-R1-C0",
    "rcll" -> "002-R2-C0",
  )

  override def run(options: PddlBenchmarksOptions, remainingArgs: RemainingArgs): Unit = {
    implicit val opts: PddlBenchmarksOptions = options
    dahu.utils.debug.LOG_LEVEL = options.verbose
    println("Warming up")
    warmup()

    println("Starting")
    for((d, p) <- defaults) {
//      println("old: " + solveOld(d, p).format)
      println("new: " + solve(d, p).format)
    }
  }

  def warmup()(implicit opts: PddlBenchmarksOptions): Unit = {
    dahu.utils.debug.LOG_LEVEL = 2
    solve("blocks_ipc2", "p04-0")
    solve("blocks_ipc2", "p04-1")
    dahu.utils.debug.LOG_LEVEL = opts.verbose
  }

  def solve(domName: String, pbName: String)(implicit opts: PddlBenchmarksOptions): Run = {
    val PddlProblem(dom, pb) = Extractor.extract(domName, pbName).getOrElse(fail())
    val start = System.currentTimeMillis().millis
    val res = Main.solve(dom, pb, Config(maxRuntime = opts.runtime.seconds, minInstances = 2))
    val end = System.currentTimeMillis().millis
    System.gc()

    res match {
      case Right(_) => Run(domName, pbName, true, end - start)
      case Left(_)  => Run(domName, pbName, false, end - start)
    }
  }

  def solveOld(domName: String, pbName: String)(implicit opts: PddlBenchmarksOptions): Run = {
    val PddlProblem(dom, pb) = Extractor.extract(domName, pbName).getOrElse(fail())
    val start = System.currentTimeMillis().millis
    val res = Main.solveOld(dom, pb, Config(maxRuntime = opts.runtime.seconds, minInstances = 2))
    val end = System.currentTimeMillis().millis
    System.gc()

    res match {
      case Right(_) => Run(domName, pbName, true, end - start)
      case Left(_)  => Run(domName, pbName, false, end - start)
    }
  }

}
