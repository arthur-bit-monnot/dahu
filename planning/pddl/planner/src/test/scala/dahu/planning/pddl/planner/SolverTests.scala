package dahu.planning.pddl.planner

import dahu.planning.pddl.problems._
import utest._

import scala.concurrent.duration._

object SolverTests extends TestSuite {

  private def fail(): Unit = assert(false)

  def findPlan(runtime: Int = 15)(implicit testPath: utest.framework.TestPath): Unit = {
    val Seq(domName, pbName) = testPath.value.takeRight(2)
    val PddlProblem(dom, pb) = Extractor.extract(domName, pbName).getOrElse(fail())
    assertMatch(Main.solve(dom, pb, Config(maxRuntime = runtime.seconds))) {
      case Right(plan) =>
    }
  }

  override def tests = Tests {
    "extraction" - {
      assertMatch(Extractor.extract("rovers_ipc5", "p01")) {
        case Right(_) =>
      }
    }

    dahu.utils.debug.LOG_LEVEL = 2

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
