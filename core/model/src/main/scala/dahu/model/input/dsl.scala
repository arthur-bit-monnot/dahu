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
    def apply(arg: Expr[I]): Expr[O] = Computation1(lhs, arg)
  }

  implicit class Fun2Ops[I1, I2, O: Tag](f: Fun2[I1, I2, O]) {
    def apply(i1: Expr[I1], i2: Expr[I2]): Computation2[I1, I2, O] =
      Computation(f, i1, i2)
  }

  def ITE[T](cond: Expr[Boolean], t: Expr[T], f: Expr[T]) =
    new ITE[T](cond, t, f)

  implicit def double2Cst(value: Double): Expr[Double] = Cst(value)
  implicit def int2Cst(value: Int): Expr[Int] = Cst(value)

  implicit class InputHelper(val sc: StringContext) extends AnyVal {
    def d(args: Any*): Expr[Double] = Input[Double](sc.s(args: _*))
  }

  implicit val boolLike: BoolLike[Expr[Boolean]] = new BoolLike[Expr[Boolean]] {
    override def and(a: Expr[Boolean], b: Expr[Boolean]): Expr[Boolean] =
      Computation(bool.And, Seq(a, b))
    override def or(a: Expr[Boolean], b: Expr[Boolean]): Expr[Boolean] =
      Computation(bool.Or, Seq(a, b))
    override def not(a: Expr[Boolean]): Expr[Boolean] =
      Computation(bool.Not, a)

    override def False: Expr[Boolean] = bool.True
    override def True: Expr[Boolean] = bool.False
  }

  implicit val intsNumber: NumberLike.Aux[Expr[Int], Expr[Boolean], Expr[Int]] =
    Numeric.toNumberLike[Int, Expr]

  implicit val numbersDouble: NumberLike.Aux[Expr[Double], Expr[Boolean], Expr[Double]] =
    Numeric.toNumberLike[Double, Expr]

  implicit def orderableIsoInt[T: TagIsoInt]: Orderable.Aux[Expr[T], Expr[Boolean]] =
    new Orderable[Expr[T]] {
      override type Bool = Expr[Boolean]
      override def BL: BoolLike[Expr[Boolean]] = boolLike
      override def leq(a: Expr[T], b: Expr[T]): Bool =
        intsNumber.leq(TagIsoInt[T].unbox(a), TagIsoInt[T].unbox(b))
      override def lt(a: Expr[T], b: Expr[T]): Bool =
        intsNumber.lt(TagIsoInt[T].unbox(a), TagIsoInt[T].unbox(b))
    }

  implicit def toNumberLikeOps[A: NumberLike](a: A) = new algebra.NumberLike.NumberLikeOps(a)
  implicit def toOrderableLikeOps[A: Orderable](a: A) = new core.algebra.Orderable.OrderableOps(a)
  implicit def toBoolLikeOps[A: BoolLike](a: A) = new core.algebra.BoolLike.BoolLikeOps(a)

  implicit final class ToSubjectToOps[F[_], T](private val lhs: F[T])(
      implicit ev: F[T] <:< Expr[T]) {

    def subjectTo(cond: F[T] => Expr[Boolean]): Expr[T] =
      SubjectTo(lhs, cond(lhs))

  }

  implicit class UnboxOps[A](private val lhs: Expr[A]) extends AnyVal {
    def unboxed(implicit tag: TagIsoInt[A]): Expr[Int] =
      Computation(tag.unbox, lhs)
  }

  implicit class ProductOps[T[_[_]]](private val lhs: Product[T]) extends AnyVal {

    def subjectTo(cond: Product[T] => Expr[Boolean]): SubjectTo[T[cats.Id]] =
      SubjectTo(lhs, cond(lhs))
  }

  implicit final class SubjectToOps[T](private val lhs: SubjectTo[T]) extends AnyVal {
    implicit private[this] def tag: Tag[T] = lhs.typ

    /** Swallows a violated constraint and uses the provided value instead. */
    def recover(onFailure: Expr[T]): ITE[T] =
      ITE(
        lhs.condition,
        lhs.value,
        onFailure
      )

  }

  implicit final class OptionalOps[T](private val lhs: Optional[T]) {
    implicit private[this] def tag: Tag[T] = lhs.typ
    def subjectTo(f: Expr[T] => Expr[Boolean]): SubjectTo[T] = {
      SubjectTo(lhs, lhs.present ==> f(lhs.value))
    }

    def embed: ITE[Option[T]] =
      ITE(lhs.present, lhs.value.map(x => Option(x)), Cst(None))

    def orElse(v: Expr[T]): ITE[T] =
      ITE(
        lhs.present,
        lhs.value,
        v
      )
  }

  implicit final class TentativeOps[T](private val lhs: Expr[T]) extends AnyVal {
    implicit private[this] def tag: Tag[T] = lhs.typ
    def map[B](f: Fun1[T, B]): Expr[B] = Computation1(f, lhs)
    def map[B: Tag](f: T => B): Expr[B] = Computation1(Fun1.embed(f), lhs)

    def subjectTo(cond: Expr[T] => Expr[Boolean]): Expr[T] =
      SubjectTo(lhs, cond(lhs))
  }

  implicit class BooleanExprOps(a: Expr[Boolean]) {
    def toDouble: Expr[Double] = ITE(a, Cst(1.0), Cst(0.0))
    def toInt: Expr[Int] = ITE(a, Cst(1), Cst(0))
  }
}
