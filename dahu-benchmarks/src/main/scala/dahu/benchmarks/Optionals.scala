package dahu.benchmarks

import dahu.model.input._
import dahu.model.input.dsl._
import dahu.solvers.PartialSolver
import dahu.solvers.constraints.CSPPartialSolver
import dahu.z3.Z3PartialSolver

object Optionals extends Family("optionals") {

  override def defaultSolver: PartialSolver.Builder = Z3PartialSolver.builder

  def boolVar() =
    Input[Boolean]()

//  instances("variables") {
//    def variable() =
//      Input[Int]().subjectTo(x => x >= 0 && x <= 0)
//
//    val itv1 = Optional(variable(), boolVar())
//    val itv2 = Optional(variable(), boolVar())
//
//    val e1 =
//      Product.fromSeq(Seq(itv1, itv2).map(_.embed)).subjectTo(_ => itv1.present || itv2.present)
//    val e2 =
//      Product.fromSeq(Seq(itv1, itv2).map(_.embed)).subjectTo(_ => itv1.present && itv2.present)
//
//    Seq(
//      SatProblem.fromExpr(e1, NumSolutions.AtLeast(3)),
//      SatProblem.fromExpr(e2, NumSolutions.AtLeast(1)),
//    )
//  }

//  instances("interval") {
//    def variable() =
//      Input[Int]().subjectTo(x => x >= 0 && x <= 10)
//
//    case class Interval[F[_]](start: F[Int], end: F[Int])
//
//    def itv() =
//      Product(Interval(variable(), variable())).subjectTo(itv => itv.value.start < itv.value.end)
//
//    val itv1 = Optional(itv(), boolVar())
//    val itv2 = Optional(itv(), boolVar())
//
//    val e1 =
//      Product.fromSeq(Seq(itv1.embed, itv2.embed)).subjectTo(_ => itv1.present || itv2.present)
//    val e2 =
//      Product.fromSeq(Seq(itv1, itv2).map(_.embed)).subjectTo(_ => itv1.present && itv2.present)
//
//    Seq(
//      SatProblem.fromExpr(e1, NumSolutions.AtLeast(3)),
//      SatProblem.fromExpr(e2, NumSolutions.AtLeast(1)),
//    )
//  }

  instances("opt-comp") {
    val x = Input[Int]("x").subjectTo(x => x < 10)
    val y = Input[Int]("y").subjectTo(y => y >= 0 && y <= 10)

    val ox = Optional(x, Input[Boolean]("px"))
    val oy = Optional(y, Input[Boolean]("py").subjectTo(_ === Cst(true)))

    val pb = Product.fromSeq(Seq(ox, oy).map(_.embed)).subjectTo(_ => ox <= oy)
    println(pb)

    Seq(
      SatProblem.fromExpr(pb, NumSolutions.AtLeast(1)),
    )
  }
}
