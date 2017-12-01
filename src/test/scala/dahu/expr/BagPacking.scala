package dahu.expr


import dahu.recursion.{Algebras, Val}
import org.scalatest.FreeSpec

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

  def evalBind(instantiations: Boolean*): Option[Double] = {
    val binds = decisions.zip(instantiations).map{ case (variable, value)=> variable.bind(value) }
    Algebras.evaluate(valid, binds).toOption match {
      case Some(true) =>
        Algebras.evaluate(utility, binds).toOption
      case x => None
    }
  }

  "expected utility" in {
    assert(evalBind(false, false) === Some(0.0))
    assert(evalBind(false, true) === Some(2.7))
    assert(evalBind(true, false) === Some(2.0))
    assert(evalBind(true, true) === None)
  }
}
