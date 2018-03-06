package dahu.model.input

import dahu.model.functions.{Fun, Fun2}
import dahu.model.math.BooleanLike.BooleanOps
import dahu.model.math.Numeric.{NumericBase, NumericOps}
import dahu.model.math._
import dahu.model.types.{TTag, Tag, TagIsoInt}

import scala.language.implicitConversions

object dsl {

  implicit class Fun2Ops[I1, I2, O: TTag](f: Fun2[I1, I2, O]) {
    def apply(i1: Tentative[I1], i2: Tentative[I2]): Computation2[I1, I2, O] =
      Computation(f, i1, i2)
  }

  def IF[T: TTag](cond: Tentative[Boolean], t: Tentative[T], f: Tentative[T]) =
    Computation(new dahu.model.math.bool.If[T], cond, t, f)

  implicit def double2Cst(value: Double): Cst[Double] = Cst(value)
  implicit def int2Cst(value: Int): Cst[Int] = Cst(value)

  implicit class InputHelper(val sc: StringContext) extends AnyVal {
    def d(args: Any*): Input[Double] = Input[Double](sc.s(args: _*))
  }

  // todo: check is this is actually needed
  implicit def input2Expr[T](input: Input[T]): Tentative[T] = input
  implicit def cst2Expr[T](cst: Cst[T]): Tentative[T] = cst
  implicit def value2Expr[T: NumericBase: Tag](value: T): Tentative[T] = Cst(value)

  implicit def tentative2numOps[T](lhs: Tentative[T])(implicit num: Numeric[T, Tentative],
                                                      bool: BooleanLike[Boolean, Tentative],
                                                      tag: Tag[T]) =
    new NumericOps[T, Tentative](lhs)

  implicit def tentative2boolOps(lhs: Tentative[Boolean])(
      implicit bool: BooleanLike[Boolean, Tentative]) =
    new BooleanOps[Boolean, Tentative](lhs)(bool)

  trait WrappedFunction {
    def f: Fun[_]
  }
  case class WrappedFun2[I1: TagIsoInt, I2: TagIsoInt, O: TagIsoInt](f: Fun2[Int, Int, O])
      extends Fun2[I1, I2, O]
      with WrappedFunction {
    override def of(in1: I1, in2: I2): O = f.of(TagIsoInt[I1].toInt(in1), TagIsoInt[I2].toInt(in2))
    override def name: String = s"wrapped-${f.name}"
  }
  object OrderIsoIntOps {
    def wrap[T, O](f: Fun2[Int, Int, O])(implicit tag: TagIsoInt[T],
                                         outTag: TagIsoInt[O]): Fun2[T, T, O] =
      new WrappedFun2[T, T, O](f)
  }

  implicit class SubjectToOps[F[_], T](private val lhs: F[T])(implicit ev: F[T] <:< Tentative[T]) {

    def subjectTo(cond: F[T] => Tentative[Boolean]): Tentative[T] =
      SubjectTo(lhs, cond(lhs))
  }
  implicit class SubjectToOps2[T[_[_]]](val lhs: Product[T]) {

    def subjectTo(cond: Product[T] => Tentative[Boolean]): Tentative[T[cats.Id]] =
      SubjectTo(lhs, cond(lhs))
  }

  implicit class BooleanExprOps(a: Tentative[Boolean]) {
    def toDouble: Tentative[Double] = IF(a, Cst(1.0), Cst(0.0))
    def toInt: Tentative[Int] = IF(a, Cst(1), Cst(0))
  }
}
