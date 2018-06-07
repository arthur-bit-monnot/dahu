package dahu.planning.pddl.planner

import dahu.model.validation.Validation
import dahu.planning.pddl.parser.{Options, Parser, PddlPredef}
import dahu.planning.pddl.problems.{Extractor, PddlProblem}
import dahu.planning.planner.chronicles.Planner
import utest._

import scala.util.{Failure, Success}

object EncodingTests extends TestSuite {

  private def fail(): Unit = assert(false)

  def checkEncoding(numFuzz: Int = 10)(implicit testPath: utest.framework.TestPath): Unit = {
    val Seq(domName, pbName) = testPath.value.takeRight(2)
    val PddlProblem(dom, pb) = Extractor.extract(domName, pbName).getOrElse(fail())

    val pddlOptions = Options()
    val parser = new Parser()(pddlOptions)

    implicit val predef: PddlPredef = parser.predef

    parser.parse(dom, pb) match {
      case Success(model) =>
        val e = Planner.asChronicleExpr(model, _ => 2, symBreak = true)
        Validation.deepFuzzedEvalCheck(e, numFuzz)
      case Failure(e) => throw e
    }
  }

  override def tests = Tests {
    "fuzzing" - {
      "blocks_ipc2" - {
        "p04-0" - checkEncoding()
      }
      "rovers_ipc5" - {
        "p01" - checkEncoding()
      }
    }
  }

}
