package dahu.model.input

import dahu.maps.ArrayMap
import dahu.model.compiler.Algebras
import dahu.model.interpreter.Interpreter
import dahu.model.ir.InputF
import dahu.model.types._
import dahu.utils.Errors.Error
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

  val w: Expr[Double] = w1 * x1.toDouble + w2 * x2.toDouble
  val valid: Expr[Boolean] = w <= W
  val utility: Expr[Double] = p1 * x1.toDouble + p2 * x2.toDouble

  val decisions = List(x1, x2)

  val possibleBinds = List(
    List(false, false),
    List(false, true),
    List(true, false),
    List(true, true)
  )
  def tests = Tests {
    val ast = Algebras.parse(valid)
    "all-true" - {
      val satisfied = Interpreter.eval(ast)(_ => true)
      assert(satisfied == false)
    }
    "all-false" - {
      val satisfied = Interpreter.eval(ast)(_ => false)
      assert(satisfied == true)
    }

    //  def evalBind(instantiations: Boolean*): Validation[Throwable, Double] = {
    //    val binds = decisions.zip(instantiations).map {
    //      case (variable, value) => variable.bind(value)
    //    }
    //    Algebras.evaluate(valid, binds).leftMap(_.head) match {
    //      case Success(true) =>
    //        Algebras.evaluate(utility, binds).leftMap(_.head)
    //      case Success(false) => Failure(Error("Invalid instance"))
    //      case Failure(x)     => Failure(x)
    //    }
    //  }

    //  "expected utility" in {
    //    assert(evalBind(false, false) == Success(0.0))
    //    assert(evalBind(false, true) == Success(2.7))
    //    assert(evalBind(true, false) == Success(2.0))
    //    assert(evalBind(true, true) == Failure(Error("Invalid instance")))
    //  }

    "running program" - {
      //    import dahu.interpreter._
      //    val ast = Algebras.encode(valid)
      //
      //    assert(evaluate(ast, Environment("x1"         -> true, "x2" -> false)) == Right(true))
      //    assert(evaluate(ast, Environment("x1"         -> true, "x2" -> true)) == Right(false))
      //    assert(forward.evaluate(ast, Environment("x1" -> true, "x2" -> false)) == Right(true))
      //    assert(forward.evaluate(ast, Environment("x1" -> true, "x2" -> true)) == Right(false))
    }
  }
}
