package dahu.benchmarks

import dahu.model.input._
import dahu.model.input.dsl._

object Optionals extends Family("optionals") {
  var counter = 0
  def variable() =
    Input[Int]({ counter += 1; "v" + counter.toString }) //.subjectTo(x => x >= 0 && x <= 0)

  def boolVar() = Input[Boolean]({ counter += 1; "v" + counter.toString })

  case class Interval[F[_]](start: F[Int], end: F[Int])

//  def itv(): Tentative[Interval[cats.Id]] =
//    Product(Interval(variable(), variable())).subjectTo(itv => itv.value.start < itv.value.end)

  val itv1 = Optional(variable(), boolVar())
  val itv2 = Optional(variable(), boolVar())

  val e1 = Product.fromSeq(Seq(itv1, itv2)).subjectTo(_ => itv1.present || itv2.present)
  val e2 = Product.fromSeq(Seq(itv1, itv2)).subjectTo(_ => itv1.present && itv2.present)

  instances("base")(
    Seq(
      SatProblem.fromExpr(e1, NumSolutions.AtLeast(3)),
      SatProblem.fromExpr(e2, NumSolutions.AtLeast(1)),
    )
  )
}
