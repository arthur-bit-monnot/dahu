package dahu.model

import cats.Id
import dahu.model.compiler.Algebras
import dahu.model.functions.->:
import dahu.model.input._
import dahu.model.ir._
import dahu.model.math.{bool, int}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types.Tag._
import dahu.model.types._
import dahu.model.validation.Validation
import dahu.recursion.FAlgebra
import utest._

object DynamicTests extends TestSuite {
  import dsl._

  def tests = Tests {
    "e" - {

      def equal(i: Expr[Int]): Expr[Int ->: Boolean] =
        Lambda[Int, Boolean]((j: Expr[Int]) => i === j)

      val True = Cst(true)
      val x = Cst(1)
      val y = Cst(2)
      val dec = Input[Int]("decision")

      val xProvider = DynamicProvider(True, x)
      val yProvider = DynamicProvider(True, y)
      val zProvider = DynamicProvider(True, Cst(4))
      val aProvider = DynamicProvider(True, Cst(5))
      val inProvided = Dynamic[Int, Boolean](equal(dec), bool.Or, None)

      val result =
        SubjectTo(dec,
                  Computation(bool.And,
                              Seq(inProvided, xProvider, yProvider, zProvider, aProvider)))

      "validation" - {
        import dahu.model.interpreter.LambdaInterpreter._
//        "positive" - {
//          Validation.assertEvaluatesTo(result, _ => Some(1))(Res(1))
//          Validation.assertEvaluatesTo(result, _ => Some(2))(Res(2))
//        }
        "negative" - {
//          Validation.assertEvaluatesTo(result, _ => Some(0))(ConstraintViolated)
          Validation.assertEvaluatesTo(result, _ => Some(3))(ConstraintViolated)
//          Validation.assertEvaluatesTo(result, _ => Some(6))(ConstraintViolated)
        }
      }

//      "eval" - {
//        import dahu.model.interpreter.Interpreter.evalAlgebra
//        import dahu.model.problem.API._
//        val prepro = parseAndProcess(result, Algebras.coalgebra)
//        println(prepro.mapExternal[Id](_.valid).fullTree)
//        println(prepro.mapExternal[Id](_.value).fullTree)
//
//        prepro.eval(evalAlgebra(_ => Value(1))) ==> IR(1, true, true)
//        prepro.eval(evalAlgebra(_ => Value(2))) ==> IR(2, true, true)
//
//        prepro.eval(evalAlgebra(_ => Value(0))) ==> IR(0, true, false)
//        prepro.eval(evalAlgebra(_ => Value(3))) ==> IR(3, true, false)
//        prepro.eval(evalAlgebra(_ => Value(6))) ==> IR(6, true, false)
//      }
    }

  }

}
