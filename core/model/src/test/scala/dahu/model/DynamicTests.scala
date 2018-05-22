package dahu.model

import cats.Id
import dahu.model.compiler.Algebras
import dahu.model.functions.->
import dahu.model.input._
import dahu.model.math.{bool, int}
import dahu.model.types.Tag._
import utest._

object DynamicTests extends TestSuite {
  import dsl._

  def tests = Tests {
    "e" - {

      val equal: Expr[Int -> (Int -> Boolean)] =
        Lambda[Int, Int -> Boolean](i => Lambda[Int, Boolean](j => i === j).named("par-eq"))
          .named("full-eq")

      val True = Cst(true)
      val x = Cst(1)
      val y = Cst(2)
      val dec = Input[Int]("decision")

      val xProvider = DynamicProvider(True, x)
      val yProvider = DynamicProvider(True, y)
      val inProvided = Dynamic[Int, Int, Boolean](dec, equal, bool.Or)

      val result =
        SubjectTo(dec, Computation(bool.And, Seq(inProvided, xProvider, yProvider)))

      import dahu.model.problem.API._
      val prepro = parseAndPreProcess(result, Algebras.coalgebra)
      println(prepro.mapExternal[Id](_.valid).fullTree)
      println(prepro.mapExternal[Id](_.value).fullTree)

      assert(true)
    }
  }
}
