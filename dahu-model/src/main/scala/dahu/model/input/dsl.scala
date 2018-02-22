package dahu.model.input

import algebra.instances.BooleanAlgebra
import algebra.ring.{AdditiveSemigroup, MultiplicativeSemigroup}
import dahu.model.functions.Fun2
import dahu.model.math.BooleanLike.BooleanOps
import dahu.model.math.Numeric.NumericOps
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
  implicit def int2Cst(value: Int): Cst[Int] = Cst(value)

  implicit class InputHelper(val sc: StringContext) extends AnyVal {
    def d(args: Any*): Input[Double] = Input[Double](sc.s(args: _*))
  }

  implicit class CommonOps[T: WTypeTag](val expr: Expr[T]) {
    def subjectTo(condition: Expr[T] => Expr[Boolean]): SubjectTo[T] =
      SubjectTo(expr, condition(expr))

    def asOptional: SubjectTo[T] = SubjectTo(expr, Cst(true))
  }
//  implicit def expr2Opt[T: WTypeTag](expr: Expr[T]): SubjectTo[T] = expr.asOptional

//  implicit class SubjectToIntOps(val lhs: SubjectTo[Int]) {
//    def <=(rhs: SubjectTo[Int]): SubjectTo[Boolean] =
//      SubjectTo(lhs.value <= rhs.value, lhs.condition && rhs.condition)
//  }
//  implicit class SubjectToBoolOps(val lhs: SubjectTo[Boolean]) {
//    def &&(rhs: SubjectTo[Boolean]): Expr[Boolean] =
//      (lhs.condition && rhs.condition) ==> (lhs.value && rhs.value)
//
//    def ||(rhs: SubjectTo[Boolean]): Expr[Boolean] =
//      (lhs.condition && rhs.condition) ==> (lhs.value || rhs.value)
//  }


  implicit def expr2numOps[T](lhs: Expr[T])(implicit num: Numeric[T,Expr], bool: BooleanLike[Boolean, Expr]) =
    new NumericOps[T, Expr](lhs)(num, bool)
  implicit def subject2numOps[T](lhs: SubjectTo[T])(implicit num: Numeric[T,SubjectTo], bool: BooleanLike[Boolean, SubjectTo]) =
    new NumericOps[T, SubjectTo](lhs)(num, bool)

  implicit def expr2boolOps(lhs: Expr[Boolean])(implicit bool: BooleanLike[Boolean, Expr]) =
    new BooleanOps[Boolean, Expr](lhs)(bool)
  implicit def subject2boolOps(lhs: SubjectTo[Boolean])(implicit bool: BooleanLike[Boolean, SubjectTo]) =
    new BooleanOps[Boolean, SubjectTo](lhs)(bool)



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
//  implicit class OrderIntOps(a: Expr[Int]) {
//    def <=(b: Expr[Int]): Expr[Boolean] = Computation(int.LEQ, a, b)
//    def <(b: Expr[Int]): Expr[Boolean] = a <= b - 1
//    def >=(b: Expr[Int]): Expr[Boolean] = b <= a
//    def >(b: Expr[Int]): Expr[Boolean] = b < a
//    def ===(b: Expr[Int]): Expr[Boolean] = Computation(int.EQ, a, b)
//    def =!=(b: Expr[Int]): Expr[Boolean] = ~(a === b)
//    def +(b: Expr[Int]): Expr[Int] = Computation(int.Add, a, b)
//    def -(b: Expr[Int]): Expr[Int] = a + (-b)
//    def unary_-(): Expr[Int] = Computation(int.Negate, a)
//  }
  implicit class OrderIsoIntOps[T](a: Expr[T])(implicit tag: TagIsoInt[T]) {
    def ===(b: Expr[T]): Expr[Boolean] = Computation(OrderIsoIntOps.wrap[T, Boolean](int.EQ), a, b)
    def =!=(b: Expr[T]): Expr[Boolean] = Computation(OrderIsoIntOps.wrap[T, Boolean](int.NEQ), a, b)
  }
  case class WrappedFun2[I1: TagIsoInt, I2: TagIsoInt, O: TagIsoInt](f: Fun2[Int, Int, O])
      extends Fun2[I1, I2, O] {
    override def of(in1: I1, in2: I2): O = f.of(TagIsoInt[I1].toInt(in1), TagIsoInt[I2].toInt(in2))
    override def name: String = s"wrapped-${f.name}"
  }
  object OrderIsoIntOps {
    def wrap[T, O](f: Fun2[Int, Int, O])(implicit tag: TagIsoInt[T],
                                         outTag: TagIsoInt[O]): Fun2[T, T, O] = new Fun2[T, T, O] {
      override def of(in1: T, in2: T): O = f.of(tag.toInt(in1), tag.toInt(in2))
      override def name: String = s"wrapped-${f.name}"
    }
  }
//  implicit class OrderDoubleOps(a: Expr[Double]) {
//    def <=(b: Expr[Double]): Expr[Boolean] = Computation(double.LEQ, a, b)
//    def >=(b: Expr[Double]): Expr[Boolean] = b <= a
//    def ===(b: Expr[Double]): Expr[Boolean] = a <= b && b <= a
//    def *(b: Expr[Double]): Expr[Double] = Computation(double.Mul, a, b)
//    def +(b: Expr[Double]): Expr[Double] = Computation(double.Add, a, b)
//  }

  implicit class BooleanExprOps(a: Expr[Boolean]) {
    def toDouble: Expr[Double] = IF(a, Cst(1.0), Cst(0.0))
    def toInt: Expr[Int] = IF(a, Cst(1), Cst(0))
  }
//
//  // common algebra instances that we want to have by default
//  implicit val doubleAlgebra: DoubleAlgebra = spire.std.double.DoubleAlgebra
//  implicit val intAlgebra: IntAlgebra = spire.std.int.IntAlgebra
//  implicit val boolAlgebra: BooleanAlgebra = algebra.instances.boolean.booleanAlgebra

}
