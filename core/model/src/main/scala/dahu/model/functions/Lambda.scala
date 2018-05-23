package dahu.model.functions

import dahu.model.input.{Computation, Computation2, Cst, Expr}
import dahu.model.math.{bool, int}
import dahu.model.types.Tag
import dahu.model.types.Tag._
import dahu.model.input._
import dsl._

object Lambdas {

  val NonZero: Expr[Int ->: Boolean] = Lambda(i => !(i === Cst(0)))

  val Max = Lambda[Int, Int ->: Int](l => Lambda[Int, Int](r => dahu.model.input.ITE(l <= r, r, l)))
  val Min: Expr[Int ->: (Int ->: Int)] = Lambda(
    l => Lambda(r => dahu.model.input.ITE(l >= r, r, l)))

}
