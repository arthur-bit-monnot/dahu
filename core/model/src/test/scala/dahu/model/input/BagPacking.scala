package dahu.model.input

import dahu.model.compiler.Algebras
import dahu.model.interpreter.{FEval, Interpreter}
import dahu.model.interpreter.Interpreter.Res
import dahu.model.problem.API
import dahu.model.types._
import dahu.model.validation.Validation
import dahu.utils._
import utest._

/** TODO: (pure)
object BagPacking extends TestSuite {



  import dsl._

  val zero = Cst(0.0)

  val w1 = Cst(2.0)
  val p1 = Cst(2.0)
  val x1 = Input[Boolean]("x1")

  val w2 = Cst(2.0)
  val p2 = Cst(2.7)
  val x2 = Input[Boolean]("x2")

  val W = Cst(3.0) // max allowed weight

  val w: Expr[Double] = w1 * x1.toDouble + w2 * x2.toDouble
  val valid: Expr[Boolean] = w <= W
  val utility: Expr[Double] = p1 * x1.toDouble + p2 * x2.toDouble

  val decisions = List(x1, x2)

  def tests = Tests {

    "fuzzing" - {
      "shallow" - {
        "valid" - Validation.fuzzedEvalCheck(valid)
        "utility" - Validation.fuzzedEvalCheck(utility)
      }
      "deep" - {
        "valid" - Validation.deepFuzzedEvalCheck(valid)
        "utility" - Validation.deepFuzzedEvalCheck(utility)
      }
    }

    "satisfaction" - {
      val ast = Algebras.parse(valid)
      "all-true" - {
        Interpreter.eval(ast)(_ => Value(true)) ==> Some(false)
        evalTotal(valid, _ => Value(true))      ==> IR(false, true, true).smap(FEval(_))
      }
      "all-false" - {
        Interpreter.eval(ast)(_ => Value(false)) ==> Some(true)
        evalTotal(valid, _ => Value(false))      ==> IR(true, true, true).smap(FEval(_))
      }

      "predefined-results" - {

        val possibleBinds = Map(
          Map("x1" -> false, "x2" -> false) -> Some(true),
          Map("x1" -> false, "x2" -> true) -> Some(true),
          Map("x1" -> true, "x2" -> false) -> Some(true),
          Map("x1" -> true, "x2" -> true) -> Some(false),
        )
        for((inputs, expected) <- possibleBinds) {
          val valueOf: ast.VID => Value = id => Value(inputs(ast.variables(id).id.id.toString))
          val result = Interpreter.eval(ast)(valueOf)
          result ==> expected
        }
      }
    }
    "evaluation-subject-to" - {
      val problem = utility.subjectTo(_ => valid)
      val ast = Algebras.parse(problem)
      "all-true" - {
        Interpreter.eval(ast)(_ => Value(true)) ==> None
        evalTotal(problem, _ => Value(true))    ==> IR(4.7, true, false).smap(FEval(_))
      }
      "all-false" - {
        Interpreter.eval(ast)(_ => Value(false)) ==> Some(0.0)
        evalTotal(problem, _ => Value(false))    ==> IR(0.0, true, true).smap(FEval(_))
      }

      "predefined-results" - {

        val possibleBinds = Map(
          Map("x1" -> false, "x2" -> false) -> Res(0.0),
          Map("x1" -> false, "x2" -> true) -> Res(2.7),
          Map("x1" -> true, "x2" -> false) -> Res(2.0),
          Map("x1" -> true, "x2" -> true) -> Interpreter.ConstraintViolated
        )
        for((inputs, expected) <- possibleBinds) {
          val valueOf: ast.VID => Value = id => Value(inputs(ast.variables(id).id.id.toString))
          val result = Interpreter.evalWithFailureCause(ast)(valueOf)
          result ==> expected
        }
      }

    }
  }

}
  */
