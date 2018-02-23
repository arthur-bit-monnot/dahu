package dahu.model.input

import dahu.model.compiler.Algebras
import dahu.model.interpreter.Interpreter
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

  val possibleBinds = Map(
    Map("x1" -> false, "x2" -> false) -> Right(true),
    Map("x1" -> false, "x2" -> true) -> Right(true),
    Map("x1" -> true, "x2" -> false) -> Right(true),
    Map("x1" -> true, "x2" -> true) -> Right(false),
  )
  def tests = Tests {
    val ast = Algebras.parse(valid)
    "all-true" - {
      val satisfied = Interpreter.eval(ast)(_ => Value(true))
      assert(satisfied == Right(false))
    }
    "all-false" - {
      val satisfied = Interpreter.eval(ast)(_ => Value(false))
      assert(satisfied == Right(true))
    }

    "predefined-results" - {
      for((inputs, expected) <- possibleBinds) {
        val valueOf: ast.VID => Value = id => Value(inputs(ast.variables(id).name))
        val result = Interpreter.eval(ast)(valueOf)
        result ==> expected
      }
    }

  }
}
