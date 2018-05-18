package dahu.model.functions

import dahu.model.input.{Computation, Computation2, Cst, Expr}
import dahu.model.math.{bool, int}
import dahu.model.types.Tag
import dahu.model.input._
import dsl._

object Lambdas {

//  Tag[Int => Int]
  implicit val i = Tag.functionTag[Int, Int]

  val NonZero = Lambda[Int, Boolean](i => !(i === Cst(0)))

  val Max = Lambda[Int, Int => Int](l => Lambda[Int, Int](r => dahu.model.input.ITE(l <= r, r, l)))

}
