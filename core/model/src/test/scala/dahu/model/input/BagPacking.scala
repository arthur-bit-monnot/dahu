package dahu.model.input

import dahu.model.compiler.Algebras
import dahu.model.interpreter.Interpreter
import dahu.model.interpreter.Interpreter.Res
import dahu.model.types._
import utest._

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

  val w: Tentative[Double] = w1 * x1.toDouble + w2 * x2.toDouble
  val valid: Tentative[Boolean] = w <= W
  val utility: Tentative[Double] = p1 * x1.toDouble + p2 * x2.toDouble

  val decisions = List(x1, x2)

  def tests = Tests {
    "satisfaction" - {
      val ast = Algebras.parse(valid)
      "all-true" - {
        val satisfied = Interpreter.eval(ast)(_ => Value(true))
        satisfied ==> Some(false)
      }
      "all-false" - {
        val satisfied = Interpreter.eval(ast)(_ => Value(false))
        satisfied ==> Some(true)
      }

      "predefined-results" - {

        val possibleBinds = Map(
          Map("x1" -> false, "x2" -> false) -> Some(true),
          Map("x1" -> false, "x2" -> true) -> Some(true),
          Map("x1" -> true, "x2" -> false) -> Some(true),
          Map("x1" -> true, "x2" -> true) -> Some(false),
        )
        for((inputs, expected) <- possibleBinds) {
          val valueOf: ast.VID => Value = id => Value(inputs(ast.variables(id).id.toString))
          val result = Interpreter.eval(ast)(valueOf)
          result ==> expected
        }
      }
    }
    "evaluation-subject-to" - {
      val opt = utility.subjectTo(_ => valid)
      val ast = Algebras.parse(opt)
      "all-true" - {
        val satisfied = Interpreter.eval(ast)(_ => Value(true))
        satisfied ==> None

      }
      "all-false" - {
        val satisfied = Interpreter.eval(ast)(_ => Value(false))
        satisfied ==> Some(0.0)
      }

      "predefined-results" - {

        val possibleBinds = Map(
          Map("x1" -> false, "x2" -> false) -> Res(0.0),
          Map("x1" -> false, "x2" -> true) -> Res(2.7),
          Map("x1" -> true, "x2" -> false) -> Res(2.0),
          Map("x1" -> true, "x2" -> true) -> Interpreter.ConstraintViolated(Seq(opt))
        )
        for((inputs, expected) <- possibleBinds) {
          val valueOf: ast.VID => Value = id => Value(inputs(ast.variables(id).id.toString))
          val result = Interpreter.evalWithFailureCause(ast)(valueOf)
          result ==> expected
        }
      }

    }
  }
}
