package dahu.model.functions

import dahu.model.input.{Computation, Computation2, Cst, Expr}
import dahu.model.math.{bool, int}
import dahu.model.types.Tag
import dahu.model.types.Tag._
import dahu.model.input._
import dsl._

object Lambdas {

  val NonZero: Expr[Int ->: Boolean] =
    Lambda[Int, Boolean]((i: Expr[Int]) => !(i === Cst(0)), Ident.anonymous())

  val Max = Lambda[Int, Int ->: Int](
    (l: Expr[Int]) =>
      Lambda[Int, Int]((r: Expr[Int]) => dahu.model.input.ITE(l <= r, r, l), Ident.anonymous()),
    Ident.anonymous())
  val Min: Expr[Int ->: (Int ->: Int)] = Lambda(
    l => Lambda(r => dahu.model.input.ITE(l >= r, r, l), Ident.anonymous()),
    Ident.anonymous())

}
