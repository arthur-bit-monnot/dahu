package dahu.planning.pddl.planner

import dahu.planning.pddl.problems._
import utest._

import scala.concurrent.duration._

object SolverTests extends TestSuite {

  private def fail(): Unit = assert(false)

  def findPlan(runtime: Int = 35)(implicit testPath: utest.framework.TestPath): Unit = {
    val Seq(domName, pbName) = testPath.value.takeRight(2)
    solve(domName, pbName, runtime.seconds)
  }
  def solve(domName: String, pbName: String, runtime: FiniteDuration): Unit = {
    val PddlProblem(dom, pb) = Extractor.extract(domName, pbName).getOrElse(fail())
    assertMatch(Main.solve(dom, pb, Config(maxRuntime = runtime))) {
      case Right(plan) =>
    }
  }

  override def tests = Tests {
    "extraction" - {
      assertMatch(Extractor.extract("rovers_ipc5", "p01")) {
        case Right(_) =>
      }
    }

    dahu.utils.debug.LOG_LEVEL = 3

    "warmup" - {
      solve("rovers_ipc5", "p01", 10.seconds)
      solve("blocks_ipc2", "p04-0", 10.seconds)
      solve("rovers_ipc5", "p01", 10.seconds)
      solve("blocks_ipc2", "p04-0", 10.seconds)
    }

    "rovers_ipc5" - {
      "p01" - findPlan()
      "p02" - findPlan()
      "p03" - findPlan()
    }
    "blocks_ipc2" - {
      "p04-0" - findPlan()
    }

  }

}
