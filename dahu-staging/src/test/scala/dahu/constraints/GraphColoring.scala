package dahu.constraints

import dahu.expr.{Cst, Expr, Input}
import dahu.expr.types.TagIsoInt
import dahu.expr.dsl._
import dahu.solver.{Exactly, Family, SatProblem}

import scala.language.implicitConversions

object GraphColoring extends Family("graph-coloring") {
  sealed trait Color
  object Color {
    case object Red   extends Color
    case object Green extends Color
    case object Blue  extends Color

    implicit val tag: TagIsoInt[Color] = TagIsoInt.fromEnum(Array(Red, Green, Blue))
  }
  def color(name: String): Input[Color] = Input(name)

  import Color._
  implicit def color2Cst(color: Color): Cst[Color] = Cst(color)

  instances("simple-graph") {
    val A = color("A")
    val B = color("B")
    val C = color("C")

    val sat = A =!= B && B =!= C && C =!= A
    Seq(
      SatProblem(sat, Exactly(6)),
      SatProblem(sat && A === Green, Exactly(2)),
      SatProblem(sat && A =!= Green, Exactly(4))
    )
  }

  instances("australia") {
    val WA  = color("WA")
    val NT  = color("NT")
    val SA  = color("SA")
    val Q   = color("QL")
    val NSW = color("NSW")
    val V   = color("Vic")

    val vars = Seq(SA, WA, NT, Q, NSW, V)

    val sat =
      WA =!= NT && WA =!= SA && NT =!= SA && NT =!= Q && Q =!= SA && Q =!= NSW && NSW =!= V && NSW =!= SA && V =!= SA

    Seq(
      SatProblem(sat, Exactly(6)),
      SatProblem(sat && SA === Red, Exactly(2)),
      SatProblem(sat && SA =!= Red, Exactly(4))
    )
  }

}
