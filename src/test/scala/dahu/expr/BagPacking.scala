package dahu.expr

import dahu.recursion.Algebras
import org.scalatest.FreeSpec

import scalaz.{Failure, Success, Validation}

class BagPacking extends FreeSpec {

  import dsl._

  val zero = Cst(0.0)

  val w1 = Cst(2.0)
  val p1 = Cst(2.0)
  val x1 = Input[Boolean]("x1")

  val w2 = Cst(2.0)
  val p2 = Cst(2.7)
  val x2 = Input[Boolean]("x2")

  val W = Cst(3.0) // max allowed weight

  val w: Expr[Double]       = w1 * x1.toDouble + w2 * x2.toDouble
  val valid: Expr[Boolean]  = w <= W
  val utility: Expr[Double] = p1 * x1.toDouble + p2 * x2.toDouble

  val decisions = List(x1, x2)

  val possibleBinds = List(
    List(false, false),
    List(false, true),
    List(true, false),
    List(true, true)
  )
  final case class Error(msg: String) extends Throwable(msg)

  def evalBind(instantiations: Boolean*): Validation[Throwable, Double] = {
    val binds = decisions.zip(instantiations).map {
      case (variable, value) => variable.bind(value)
    }
    Algebras.evaluate(valid, binds).leftMap(_.head) match {
      case Success(true) =>
        Algebras.evaluate(utility, binds).leftMap(_.head)
      case Success(false) => Failure(Error("Invalid instance"))
      case Failure(x)     => Failure(x)
    }
  }

  "expected utility" in {
    assert(evalBind(false, false) == Success(0.0))
    assert(evalBind(false, true) == Success(2.7))
    assert(evalBind(true, false) == Success(2.0))
    assert(evalBind(true, true) == Failure(Error("Invalid instance")))
  }

  "running program" in {
    import dahu.interpreter._
    val ast = Algebras.encode(valid)

//    assert(evaluate(ast, Environment("x1"         -> true, "x2" -> false)) == Right(true))
//    assert(evaluate(ast, Environment("x1"         -> true, "x2" -> true)) == Right(false))
//    assert(forward.evaluate(ast, Environment("x1" -> true, "x2" -> false)) == Right(true))
//    assert(forward.evaluate(ast, Environment("x1" -> true, "x2" -> true)) == Right(false))
  }

}
