package dahu.model.math

import dahu.model.functions._

trait BooleanLike[T, F[_]] {
  def and(conjuncts: F[T]*): F[T]
  def or(disjuncts: F[T]*): F[T]
  def not(term: F[T]): F[T]
}

object BooleanLike {

  def apply[T, F[_]](implicit ev: BooleanLike[T, F]): BooleanLike[T, F] = ev

  trait BooleanBase[T] {
    def and: FunN[T, T]
    def or: FunN[T, T]
    def not: Fun1[T, T]
  }

  implicit object BoolAlgebra extends BooleanBase[Boolean] {
    override def and: FunN[Boolean, Boolean] = bool.And
    override def or: FunN[Boolean, Boolean] = bool.Or
    override def not: Fun1[Boolean, Boolean] = bool.Not
  }

  implicit def instanceOfReifApp[T, F[_]](implicit ev: BooleanBase[T],
                                          F: ReifiedApplicative[F]): BooleanLike[T, F] =
    new BooleanLike[T, F] {
      override def and(conjuncts: F[T]*): F[T] =
        F.mapN(conjuncts: _*)(ev.and)
      override def or(disjuncts: F[T]*): F[T] =
        F.mapN(disjuncts: _*)(ev.or)
      override def not(term: F[T]): F[T] =
        F.map(term)(ev.not)
    }

  implicit class BooleanOps[T, F[_]](val lhs: F[T])(implicit bool: BooleanLike[T, F]) {
    def &&(rhs: F[T]): F[T] = bool.and(lhs, rhs)
    def ||(rhs: F[T]): F[T] = bool.or(lhs, rhs)
    def unary_~(): F[T] = bool.not(lhs)
    def implies(rhs: F[T]): F[T] = (~lhs) || rhs
    def ==>(rhs: F[T]): F[T] = lhs implies rhs
  }
}
