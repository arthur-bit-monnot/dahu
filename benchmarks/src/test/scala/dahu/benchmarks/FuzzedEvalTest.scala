package dahu.benchmarks

import dahu.model.validation.Validation
import utest._

object FuzzedEvalTest extends TestSuite {

  val instances = NumSolutionsTest.instances.filter(_._1 == "optionals/4-opt-comp")

  override def tests = Tests {

    "fuzzing" - {
      def test(pb: SatProblem): Unit = {
        val e = pb.pb
        Validation.deepFuzzedEvalCheck(e)
      }
      dahu.utils.tests.subtests[(String, SatProblem)](instances, x => test(x._2), _._1)
    }
  }

}
