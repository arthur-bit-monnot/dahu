package dahu.constraints

import dahu.expr.{Expr, Input}
import dahu.expr.types.TagIsoInt
import org.scalatest.FunSuite

class GraphColoring extends FunSuite {

  // https://i.ytimg.com/vi/IDG-ldeEda0/hqdefault.jpg

  trait Enum

  sealed trait Color
  object Color {
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

    implicit val tag: TagIsoInt[Color] = TagIsoInt.fromEnum(Array(Red, Green, Blue))
  }


  test("graph-australia") {
    import dahu.expr.dsl._

    def color(name: String): Input[Color] = Input(name)
    val WA = color("WA")
    val NT = color("NT")
    val SA = color("SA")
    val Q = color("QL")
    val NSW = color("NSW")
    val V = color("Victoria")
    val T = color("Tasmania")

    val vars = Seq(WA, NT, SA, Q, NSW, V, T)

    val sat = WA =!= NT && WA =!= SA && NT =!= SA && Q =!= SA && Q =!= NSW && NSW =!= V && NSW =!= SA && V =!= SA

    solve(sat)

    def solve(sat: Expr[Boolean]): Unit = {

      import dahu.recursion.Algebras._

      val asd = transpile(sat, coalgebra)
      val csp = CSP.from(asd)

      csp.solve match {
        case Some(assignment) =>
          def view[T](x: Expr[T])(implicit tag: TagIsoInt[T]): Option[T] = {
            asd.compiledForm(x)
              .map(y => assignment(y))
              .map(z => tag.fromInt(z))
          }
          for(x <- vars) {
            println(s"$x <- ${view(x)}")
          }
        case None =>
          println("No solution")
      }
    }


  }

}
