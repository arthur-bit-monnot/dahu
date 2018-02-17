package dahu.constraints

import dahu.expr.types.TagIsoInt
import dahu.expr.{bool, Expr, Input}
import dahu.recursion.Types._
import dahu.solver.MetaSolver1

object MiniCSP extends App {

  import dahu.expr.dsl._

  val x    = Input[Int]("x")
  val y    = Input[Int]("y")
  val b    = Input[Boolean]("b")
  val leq  = x <= y
  val root = leq || b

  import dahu.recursion.Algebras._

  val asd = transpile(root, coalgebra)
//  val csp = CSP.from(asd)
  val solver = new MetaSolver1[Integer](asd)

  solver.nextSolution match {
    case Some(assignment) =>
      def view[T](x: Expr[T]): Option[T] = {
        asd
          .compiledForm(x)
          .flatMap(y => assignment.get(y))
          .map(_.asInstanceOf[T])
//          .map(z => tag.fromInt(z))
      }
      println(view(root))
      println(view(leq))
      println(view(b))
      assert(view(root).contains(true))
      println(assignment)
    case None =>
  }

//  println(csp.dom)

  println("done.")

}
