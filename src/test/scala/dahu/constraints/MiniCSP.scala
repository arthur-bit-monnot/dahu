package dahu.constraints

import dahu.expr.{Input, bool}

object MiniCSP extends App {

  import dahu.expr.dsl._

  val x = Input[Int]("x")
  val y = Input[Int]("y")
  val b = Input[Boolean]("b")
  val leq = x <= y
  val root = leq || b

  import dahu.recursion.Algebras._

  val asd = transpile(root, coalgebra)
  val csp = new CSP(asd)

  println(csp.solve)

  println("done.")

}
