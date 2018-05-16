package dahu.model.input

import dahu.core
import dahu.core.algebra
import dahu.core.algebra.{BoolLike, NumberLike, Orderable}
import dahu.model.functions.{Fun, Fun1, Fun2, FunN}
import dahu.model.ir.CstF
import dahu.model.math._
import dahu.model.types.{Tag, TagIsoInt}

import scala.language.implicitConversions

object dsl {

  implicit class Fun1Ops[I, O](private val lhs: Fun1[I, O]) extends AnyVal {
    def apply(arg: Tentative[I]): Tentative[O] = Computation1(lhs, arg)
  }

  implicit class Fun2Ops[I1, I2, O: Tag](f: Fun2[I1, I2, O]) {
    def apply(i1: Tentative[I1], i2: Tentative[I2]): Computation2[I1, I2, O] =
      Computation(f, i1, i2)
  }

  def ITE[T](cond: Tentative[Boolean], t: Tentative[T], f: Tentative[T]) =
    new ITE[T](cond, t, f)

  implicit def double2Cst(value: Double): Tentative[Double] = Cst(value)
  implicit def int2Cst(value: Int): Tentative[Int] = Cst(value)

  implicit class InputHelper(val sc: StringContext) extends AnyVal {
    def d(args: Any*): Tentative[Double] = Input[Double](sc.s(args: _*))
  }

  implicit val boolLike: BoolLike[Tentative[Boolean]] = new BoolLike[Tentative[Boolean]] {
    override def and(a: Tentative[Boolean], b: Tentative[Boolean]): Tentative[Boolean] =
      Computation(bool.And, Seq(a, b))
    override def or(a: Tentative[Boolean], b: Tentative[Boolean]): Tentative[Boolean] =
      Computation(bool.Or, Seq(a, b))
    override def not(a: Tentative[Boolean]): Tentative[Boolean] =
      Computation(bool.Not, a)

    override def False: Tentative[Boolean] = bool.True
    override def True: Tentative[Boolean] = bool.False
  }

  implicit val intsNumber: NumberLike.Aux[Tentative[Int], Tentative[Boolean], Tentative[Int]] =
    Numeric.toNumberLike[Int, Tentative]

  implicit val numbersDouble
    : NumberLike.Aux[Tentative[Double], Tentative[Boolean], Tentative[Double]] =
    Numeric.toNumberLike[Double, Tentative]

  implicit def orderableIsoInt[T: TagIsoInt]: Orderable.Aux[Tentative[T], Tentative[Boolean]] =
    new Orderable[Tentative[T]] {
      override type Bool = Tentative[Boolean]
      override def BL: BoolLike[Tentative[Boolean]] = boolLike
      override def leq(a: Tentative[T], b: Tentative[T]): Bool =
        intsNumber.leq(TagIsoInt[T].unbox(a), TagIsoInt[T].unbox(b))
      override def lt(a: Tentative[T], b: Tentative[T]): Bool =
        intsNumber.lt(TagIsoInt[T].unbox(a), TagIsoInt[T].unbox(b))
    }

  implicit def toNumberLikeOps[A: NumberLike](a: A) = new algebra.NumberLike.NumberLikeOps(a)
  implicit def toOrderableLikeOps[A: Orderable](a: A) = new core.algebra.Orderable.OrderableOps(a)
  implicit def toBoolLikeOps[A: BoolLike](a: A) = new core.algebra.BoolLike.BoolLikeOps(a)

  implicit final class ToSubjectToOps[F[_], T](private val lhs: F[T])(
      implicit ev: F[T] <:< Tentative[T]) {

    def subjectTo(cond: F[T] => Tentative[Boolean]): Tentative[T] =
      SubjectTo(lhs, cond(lhs))

  }

  implicit class UnboxOps[A](private val lhs: Tentative[A]) extends AnyVal {
    def unboxed(implicit tag: TagIsoInt[A]): Tentative[Int] =
      Computation(tag.unbox, lhs)
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
