package dahu.constraints

import dahu.expr.types.TagIsoInt
import dahu.expr.{Expr, Input, bool}

object MiniCSP extends App {

  import dahu.expr.dsl._

  val x = Input[Int]("x")
  val y = Input[Int]("y")
  val b = Input[Boolean]("b")
  val leq = x <= y
  val root = leq && b

  import dahu.recursion.Algebras._

  val asd = transpile(root, coalgebra)
  val csp = CSP.from(asd)

  csp.solve match {
    case Some(assignment) =>
      def view[T](x: Expr[T])(implicit tag: TagIsoInt[T]): Option[T] = {
        asd.compiledForm(x)
          .map(y => assignment(y))
          .map(z => tag.fromInt(z))
      }
      println(view(root))
      println(view(leq))
      println(view(b))
      assert(view(root) == Some(true))
      println(assignment)
    case None =>
  }

//  println(csp.dom)

  println("done.")

}
