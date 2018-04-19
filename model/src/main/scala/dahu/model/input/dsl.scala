package dahu.model.input

import dahu.model.functions.{Fun, Fun1, Fun2}
import dahu.model.ir.Total
import dahu.model.math.BooleanLike.BooleanOps
import dahu.model.math.Numeric.{NumericBase, NumericOps}
import dahu.model.math._
import dahu.model.math.obj.Unboxed
import dahu.model.types.{BoxedInt, Tag}

import scala.language.implicitConversions

object dsl {

  implicit class Fun2Ops[I1, I2, O: Tag](f: Fun2[I1, I2, O]) {
    def apply(i1: Tentative[I1], i2: Tentative[I2]): Computation2[I1, I2, O] =
      Computation(f, i1, i2)
  }

  def ITE[T](cond: Tentative[Boolean], t: Tentative[T], f: Tentative[T]) =
    new ITE[T](cond, t, f)

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

  implicit final class ToSubjectToOps[F[_], T](private val lhs: F[T])(
      implicit ev: F[T] <:< Tentative[T]) {

    def subjectTo(cond: F[T] => Tentative[Boolean]): Tentative[T] =
      SubjectTo(lhs, cond(lhs))

  }

  implicit class UnboxOps[A](private val lhs: Tentative[A]) extends AnyVal {
    def unboxed(implicit tag: BoxedInt[A]): Tentative[Int] =
      Computation(new Unboxed[A], lhs)
  }

  implicit class ProductOps[T[_[_]]](private val lhs: Product[T]) extends AnyVal {

    def subjectTo(cond: Product[T] => Tentative[Boolean]): SubjectTo[T[cats.Id]] =
      SubjectTo(lhs, cond(lhs))
  }

  implicit final class SubjectToOps[T](private val lhs: SubjectTo[T]) extends AnyVal {
    implicit private[this] def tag: Tag[T] = lhs.typ

    /** Swallows a violated constraint and uses the provided value instead. */
    def recover(onFailure: Tentative[T]): ITE[T] =
      ITE(
        lhs.condition,
        lhs.value,
        onFailure
      )

  }

  implicit final class OptionalOps[T](private val lhs: Optional[T]) {
    implicit private[this] def tag: Tag[T] = lhs.typ
    def subjectTo(f: Tentative[T] => Tentative[Boolean]): SubjectTo[T] = {
      SubjectTo(lhs, lhs.present ==> f(lhs.value))
    }

    def embed: ITE[Option[T]] =
      ITE(lhs.present, lhs.value.map(x => Option(x)), Cst(None))

    def orElse(v: Tentative[T]): ITE[T] =
      ITE(
        lhs.present,
        lhs.value,
        v
      )
  }

  implicit final class TentativeOps[T](private val lhs: Tentative[T]) extends AnyVal {
    implicit private[this] def tag: Tag[T] = lhs.typ
    def map[B](f: Fun1[T, B]): Tentative[B] = Computation1(f, lhs)
    def map[B: Tag](f: T => B): Tentative[B] = Computation1(Fun1.embed(f), lhs)

    def subjectTo(cond: Tentative[T] => Tentative[Boolean]): Tentative[T] =
      SubjectTo(lhs, cond(lhs))
  }

  implicit class BooleanExprOps(a: Tentative[Boolean]) {
    def toDouble: Tentative[Double] = ITE(a, Cst(1.0), Cst(0.0))
    def toInt: Tentative[Int] = ITE(a, Cst(1), Cst(0))
  }
}
