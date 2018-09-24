package dahu.benchmarks

import dahu.model.types._
import dahu.model.input._
import dahu.model.input.dsl._
import NumSolutions._
import dahu.model.compiler.Algebras
import dahu.solvers.PartialSolver

import scala.language.implicitConversions

object GraphColoring extends Family("graph-coloring") {
  sealed trait Color
  object Color {
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

    implicit val tag: TagIsoInt[Color] = TagIsoInt.fromEnum(Array(Red, Green, Blue))
  }
  def color(name: String): Expr[Color] = Input(name)

  import Color._
  implicit def color2Cst(color: Color): Expr[Color] = Cst(color)

  instances("simple-graph") {
    val A = color("A")
    val B = color("B")
    val C = color("C")

    val sat = A =!= B && B =!= C && C =!= A
    Seq(
      SatProblem.fromSat(sat, 6),
      SatProblem.fromSat(sat && A === Green, 2),
      SatProblem.fromSat(sat && A =!= Green, 4)
    )
  }

  instances("australia") {
    val WA = color("WA")
    val NT = color("NT")
    val SA = color("SA")
    val Q = color("QL")
    val NSW = color("NSW")
    val V = color("Vic")

    val vars = Seq(SA, WA, NT, Q, NSW, V)

    val sat =
      WA =!= NT && WA =!= SA && NT =!= SA && NT =!= Q && Q =!= SA && Q =!= NSW && NSW =!= V && NSW =!= SA && V =!= SA

    Seq(
      SatProblem.fromSat(sat, 6),
      SatProblem.fromSat(sat && SA === Red, 2),
      SatProblem.fromSat(sat && SA =!= Red, 4)
    )
  }
}
