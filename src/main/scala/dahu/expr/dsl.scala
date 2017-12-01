package dahu.expr


import algebra.Order
import algebra.ring.{AdditiveSemigroup, CommutativeRing, Field, MultiplicativeSemigroup}
import spire.std.{DoubleAlgebra, IntAlgebra}

import scala.reflect.runtime.universe.{Type, TypeTag, typeOf}


package object dsl {


  implicit class Fun2Ops[I1, I2, O : TypeTag](f: Fun2[I1, I2, O]) {
    def apply(i1: Expr[I1], i2: Expr[I2]): Computation2[I1,I2,O] =
      Computation(f, i1, i2)
  }

  def IF[T : TypeTag](cond: Expr[Boolean], t: Expr[T], f: Expr[T]) =
    Computation(new dahu.expr.If[T], cond, t, f)

  implicit def double2Cst(value: Double): Cst[Double] = Cst(value)

  implicit class InputHelper(val sc: StringContext) extends AnyVal {
    def d(args: Any*): Input[Double] = Input[Double](sc.s(args: _*))
  }

  implicit class ResultAddOps[T: AdditiveSemigroup: TypeTag](a: Expr[T]) {
    def +(b: Expr[T]) = Computation(Math.addition2[T], a, b)
  }

  implicit class ResultMulOps[T: MultiplicativeSemigroup: TypeTag](a: Expr[T]) {
    def *(b: Expr[T]) = Computation(Math.multiplication2[T], a, b)
  }

  implicit class OrderOps[T: Order: TypeTag](a: Expr[T]) {
    def <=(b: Expr[T]) = Computation(Math.leq2[T], a, b)
  }

  implicit class BooleanOps(a: Expr[Boolean]) {
    def toDouble: Expr[Double] = IF(a, Cst(1.0), Cst(0.0))
    def toInt: Expr[Int] = IF(a, Cst(1), Cst(0))
  }


  // common algebra instances that we want to have by default
  implicit val doubleAlgebra: DoubleAlgebra = spire.std.double.DoubleAlgebra
  implicit val intAlgebra: IntAlgebra = spire.std.int.IntAlgebra

}
