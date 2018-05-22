package dahu.model

import cats.Id
import dahu.model.compiler.Algebras
import dahu.model.functions.->
import dahu.model.input._
import dahu.model.ir._
import dahu.model.math.{bool, int}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types.Tag._
import dahu.model.types._
import dahu.recursion.FAlgebra
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
      val prepro = parseAndProcess(result, Algebras.coalgebra)
      println(prepro.mapExternal[Id](_.valid).fullTree)
      println(prepro.mapExternal[Id](_.value).fullTree)

      import dahu.model.interpreter.Interpreter.evalAlgebra

      prepro.eval(evalAlgebra(_ => Value(1))) ==> IR(1, true, true)
      prepro.eval(evalAlgebra(_ => Value(2))) ==> IR(2, true, true)

      prepro.eval(evalAlgebra(_ => Value(0))) ==> IR(0, true, false)
      prepro.eval(evalAlgebra(_ => Value(3))) ==> IR(3, true, false)
      prepro.eval(evalAlgebra(_ => Value(4))) ==> IR(4, true, false)
    }

    assert(true)
  }

}
