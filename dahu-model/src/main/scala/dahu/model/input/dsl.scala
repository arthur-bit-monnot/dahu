package dahu.model.input

import algebra.instances.BooleanAlgebra
import algebra.ring.{AdditiveSemigroup, MultiplicativeSemigroup}
import dahu.model.functions.Fun2
import dahu.model.math._
import dahu.model.types.{TagIsoInt, WTypeTag}
import spire.std.{DoubleAlgebra, IntAlgebra}

import scala.language.implicitConversions

object dsl {

  implicit class Fun2Ops[I1, I2, O: WTypeTag](f: Fun2[I1, I2, O]) {
    def apply(i1: Expr[I1], i2: Expr[I2]): Computation2[I1, I2, O] =
      Computation(f, i1, i2)
  }

  def IF[T: WTypeTag](cond: Expr[Boolean], t: Expr[T], f: Expr[T]) =
    Computation(new dahu.model.math.bool.If[T], cond, t, f)

  implicit def double2Cst(value: Double): Cst[Double] = Cst(value)
  implicit def int2Cst(value: Int): Cst[Int]          = Cst(value)

  implicit class InputHelper(val sc: StringContext) extends AnyVal {
    def d(args: Any*): Input[Double] = Input[Double](sc.s(args: _*))
  }

//  implicit class ResultAddOps[T: AdditiveSemigroup: WTypeTag](a: Expr[T]) {
//    def +(b: Expr[T]) = Computation(Math.addition2[T], a, b)
//  }
//
//  implicit class ResultMulOps[T: MultiplicativeSemigroup: WTypeTag](a: Expr[T]) {
//    def *(b: Expr[T]) = Computation(Math.multiplication2[T], a, b)
//  }
//
//  implicit class OrderOps[T: Order: WTypeTag](a: Expr[T]) {
//    def <=(b: Expr[T]): Expr[Boolean]  = Computation(Math.leq2[T], a, b)
//    def >=(b: Expr[T]): Expr[Boolean]  = b <= a
//    def ===(b: Expr[T]): Expr[Boolean] = a <= b && b <= a
//  }
  implicit class OrderIntOps(a: Expr[Int]) {
    def <=(b: Expr[Int]): Expr[Boolean]  = Computation(int.LEQ, a, b)
    def >=(b: Expr[Int]): Expr[Boolean]  = b <= a
    def ===(b: Expr[Int]): Expr[Boolean] = Computation(int.EQ, a, b)
    def =!=(b: Expr[Int]): Expr[Boolean] = Computation(int.NEQ, a, b)
  }
  implicit class OrderIsoIntOps[T](a: Expr[T])(implicit tag: TagIsoInt[T]) {
    def ===(b: Expr[T]): Expr[Boolean] = Computation(OrderIsoIntOps.wrap[T, Boolean](int.EQ), a, b)
    def =!=(b: Expr[T]): Expr[Boolean] = Computation(OrderIsoIntOps.wrap[T, Boolean](int.NEQ), a, b)
  }
  case class WrappedFun2[I1: TagIsoInt, I2: TagIsoInt, O: TagIsoInt](f: Fun2[Int, Int, O])
      extends Fun2[I1, I2, O] {
    override def of(in1: I1, in2: I2): O = f.of(TagIsoInt[I1].toInt(in1), TagIsoInt[I2].toInt(in2))
    override def name: String            = s"wrapped-${f.name}"
  }
  object OrderIsoIntOps {
    def wrap[T, O](f: Fun2[Int, Int, O])(implicit tag: TagIsoInt[T],
                                         outTag: TagIsoInt[O]): Fun2[T, T, O] = new Fun2[T, T, O] {
      override def of(in1: T, in2: T): O = f.of(tag.toInt(in1), tag.toInt(in2))
      override def name: String          = s"wrapped-${f.name}"
    }
  }
  implicit class OrderDoubleOps(a: Expr[Double]) {
    def <=(b: Expr[Double]): Expr[Boolean]  = Computation(double.LEQ, a, b)
    def >=(b: Expr[Double]): Expr[Boolean]  = b <= a
    def ===(b: Expr[Double]): Expr[Boolean] = a <= b && b <= a
    def *(b: Expr[Double]): Expr[Double]    = Computation(double.Mul, a, b)
    def +(b: Expr[Double]): Expr[Double]    = Computation(double.Add, a, b)
  }

  implicit class BooleanOps(a: Expr[Boolean]) {
    def toDouble: Expr[Double]              = IF(a, Cst(1.0), Cst(0.0))
    def toInt: Expr[Int]                    = IF(a, Cst(1), Cst(0))
    def &&(b: Expr[Boolean]): Expr[Boolean] = Computation(bool.And, Array(a, b))
    def ||(b: Expr[Boolean]): Expr[Boolean] = Computation(bool.Or, Array(a, b))
    def unary_~(): Expr[Boolean]            = Computation(bool.Not, a)
  }

  // common algebra instances that we want to have by default
  implicit val doubleAlgebra: DoubleAlgebra = spire.std.double.DoubleAlgebra
  implicit val intAlgebra: IntAlgebra       = spire.std.int.IntAlgebra
  implicit val boolAlgebra: BooleanAlgebra  = algebra.instances.boolean.booleanAlgebra

}
