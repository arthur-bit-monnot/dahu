package dahu.model.math

import dahu.model.functions._
import dahu.model.types.{Tag, TagIsoInt}

trait Ordered[T, F[_]] {
  def leq(lhs: F[T], rhs: F[T]): F[Boolean]
  def eqv(lhs: F[T], rhs: F[T]): F[Boolean]
  def geq(lhs: F[T], rhs: F[T]): F[Boolean] = leq(rhs, lhs)
  def lt(lhs: F[T], rhs: F[T]): F[Boolean]
  def gt(lhs: F[T], rhs: F[T]): F[Boolean] = lt(rhs, lhs)
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
    def leq: Fun2[T, T, Boolean]
    def eqv: Fun2[T, T, Boolean]
  }

  trait NumericBase[T] extends OrderBase[T] {
    def neg: Fun1[T, T]
    def add: Monoid[T]
    def times: Monoid[T]
    def epsilonOrLesserThan: Either[T, Fun2[T, T, Boolean]]
  }

  implicit object IntNumeric extends NumericBase[Int] {
    override def leq: Fun2[Int, Int, Boolean] = int.LEQ
    override def eqv: Fun2[Int, Int, Boolean] = int.EQ
    override def add: Monoid[Int] = int.Add
    override def neg: Fun1[Int, Int] = int.Negate
    override def times: Monoid[Int] = int.Times

    override def epsilonOrLesserThan: Either[Int, Fun2[Int, Int, Boolean]] = Left(1)
  }
  implicit object DoubleNumeric extends NumericBase[Double] {
    override def leq: Fun2[Double, Double, Boolean] = double.LEQ
    override def eqv: Fun2[Double, Double, Boolean] = double.EQ
    override def add: Monoid[Double] = double.Add
    override def neg: Fun1[Double, Double] = double.Negate
    override def times: Monoid[Double] = double.Times
    override def epsilonOrLesserThan: Either[Double, Fun2[Double, Double, Boolean]] =
      Right(double.LT)
  }

  implicit def exprNumeric[T: Tag, F[_]](implicit ev: NumericBase[T],
                                         F: ReifiedApplicative[F]): Numeric[T, F] =
    new Numeric[T, F] {
      override def leq(lhs: F[T], rhs: F[T]): F[Boolean] =
        F.map2(lhs, rhs)(ev.leq)

      override def eqv(lhs: F[T], rhs: F[T]): F[Boolean] =
        F.map2(lhs, rhs)(ev.eqv)

      override def lt(lhs: F[T], rhs: F[T]): F[Boolean] =
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

  implicit class NumericOps[T, F[_]](val lhs: F[T])(implicit num: Numeric[T, F],
                                                    bool: BooleanLike[Boolean, F]) {
    def <=(rhs: F[T]): F[Boolean] = num.leq(lhs, rhs)
    def <(rhs: F[T]): F[Boolean] = num.lt(lhs, rhs)
    def >=(rhs: F[T]): F[Boolean] = num.geq(lhs, rhs)
    def >(rhs: F[T]): F[Boolean] = num.gt(lhs, rhs)
    def ===(rhs: F[T]): F[Boolean] = num.eqv(lhs, rhs)
    def =!=(rhs: F[T]): F[Boolean] = bool.not(lhs === rhs)

    def *(rhs: F[T]): F[T] = num.times(lhs, rhs)
    def +(rhs: F[T]): F[T] = num.add(lhs, rhs)
    def unary_-(): F[T] = num.negate(lhs)
    def -(rhs: F[T]): F[T] = num.add(lhs, -rhs)
  }
}
