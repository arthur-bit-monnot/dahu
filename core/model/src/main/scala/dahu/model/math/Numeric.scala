package dahu.model.math

import dahu.core.algebra.{BoolLike, NumberLike, Orderable}
import dahu.model.functions._
import dahu.model.types.{Bool, Tag, TagIsoInt}

trait Ordered[T, F[_]] {
  def leq(lhs: F[T], rhs: F[T]): F[Bool]
  def eqv(lhs: F[T], rhs: F[T]): F[Bool]
  def geq(lhs: F[T], rhs: F[T]): F[Bool] = leq(rhs, lhs)
  def lt(lhs: F[T], rhs: F[T]): F[Bool]
  def gt(lhs: F[T], rhs: F[T]): F[Bool] = lt(rhs, lhs)
}

trait Numeric[T, F[_]] extends Ordered[T, F] {

  def add(lhs: F[T], rhs: F[T]): F[T]
  def negate(term: F[T]): F[T]
  def substract(lhs: F[T], rhs: F[T]): F[T] = add(lhs, negate(rhs))

  def times(lhs: F[T], rhs: F[T]): F[T]
}

object Numeric {

  def apply[T, F[_]](implicit ev: Numeric[T, F]): Numeric[T, F] = ev

  trait OrderBase[T] {
    def leq: Fun2[T, T, Bool]
    def eqv: Fun2[T, T, Bool]
  }

  trait NumericBase[T] extends OrderBase[T] {
    def neg: Fun1[T, T]
    def add: Monoid[T]
    def times: Monoid[T]
    def epsilonOrLesserThan: Either[T, Fun2[T, T, Bool]]
  }

  implicit object IntNumeric extends NumericBase[Int] {
    override def leq: Fun2[Int, Int, Bool] = int.LEQ
    override def eqv: Fun2[Int, Int, Bool] = int.EQ
    override def add: Monoid[Int] = int.Add
    override def neg: Fun1[Int, Int] = int.Negate
    override def times: Monoid[Int] = int.Times

    override def epsilonOrLesserThan: Either[Int, Fun2[Int, Int, Bool]] = Left(1)
  }
  implicit object DoubleNumeric extends NumericBase[Double] {
    override def leq: Fun2[Double, Double, Bool] = double.LEQ
    override def eqv: Fun2[Double, Double, Bool] = double.EQ
    override def add: Monoid[Double] = double.Add
    override def neg: Fun1[Double, Double] = double.Negate
    override def times: Monoid[Double] = double.Times
    override def epsilonOrLesserThan: Either[Double, Fun2[Double, Double, Bool]] =
      Right(double.LT)
  }

  implicit def exprNumeric[T: Tag, F[_]](implicit ev: NumericBase[T],
                                         F: ReifiedApplicative[F]): Numeric[T, F] =
    new Numeric[T, F] {
      override def leq(lhs: F[T], rhs: F[T]): F[Bool] =
        F.map2(lhs, rhs)(ev.leq)

      override def eqv(lhs: F[T], rhs: F[T]): F[Bool] =
        F.map2(lhs, rhs)(ev.eqv)

      override def lt(lhs: F[T], rhs: F[T]): F[Bool] =
        ev.epsilonOrLesserThan match {
          case Left(epsilon) => leq(add(lhs, F.pure(epsilon)), rhs)
          case Right(f)      => F.map2(lhs, rhs)(f)
        }

      override def add(lhs: F[T], rhs: F[T]): F[T] =
        F.mapN(lhs, rhs)(ev.add)

      override def negate(term: F[T]): F[T] =
        F.map(term)(ev.neg)

      override def times(lhs: F[T], rhs: F[T]): F[T] =
        F.mapN(lhs, rhs)(ev.times)
    }

  def toNumberLike[T, F[_]](implicit num: Numeric[T, F],
                            bool: BoolLike[F[Bool]]): NumberLike.Aux[F[T], F[Bool], F[T]] =
    new NumberLike[F[T]] {
      override type EBool = F[Bool]
      override type Num = F[T]
      override def BL: BoolLike[EBool] = bool
      override def leq(a: F[T], b: F[T]): EBool = num.leq(a, b)
      override def lt(a: F[T], b: F[T]): EBool = num.lt(a, b)
      override def eqv(a: F[T], b: F[T]): EBool = num.eqv(a, b)

      override def times(a: F[T], b: F[T]): F[T] = num.times(a, b)
      override def add(a: F[T], b: F[T]): F[T] = num.add(a, b)
      override def sub(a: F[T], b: F[T]): F[T] = num.substract(a, b)
      override def negate(a: F[T]): F[T] = num.negate(a)
    }
}
