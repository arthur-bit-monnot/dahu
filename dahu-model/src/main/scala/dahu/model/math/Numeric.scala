package dahu.model.math

import cats.Id
import dahu.model.functions.{Fun1, Fun2, FunN}
import dahu.model.input.{Computation, Cst, Expr, SubjectTo}
import dahu.model.types.WTypeTag

trait Numeric[T, F[_]] {

  def leq(lhs: F[T], rhs: F[T]): F[Boolean]
  def geq(lhs: F[T], rhs: F[T]): F[Boolean] = leq(rhs, lhs)
  def lt(lhs: F[T], rhs: F[T]): F[Boolean]
  def gt(lhs: F[T], rhs: F[T]): F[Boolean] = lt(rhs, lhs)

  def add(lhs: F[T], rhs: F[T]): F[T]
  def negate(term: F[T]): F[T]
  def substract(lhs: F[T], rhs: F[T]): F[T] = add(lhs, negate(rhs))

  def times(lhs: F[T], rhs: F[T]): F[T]
}

object Numeric {

  trait Epsilon[T] {
    val value: T
  }
  implicit object IntEpsilon extends Epsilon[Int] {
    val value: Int = 1
  }
  implicit object DoubleEpsilon extends Epsilon[Double] {
    val value: Double = 0.000000001 // todo: do not rely on epsilon of continuous values
  }

  trait NumericBase[T] {
    def leq: Fun2[T, T, Boolean]
    def neg: Fun1[T, T]
    def add: Fun2[T, T, T]
    def times: Fun2[T, T, T]
  }

  implicit object IntNumeric extends NumericBase[Int] {
    override def leq: Fun2[Int, Int, Boolean] = int.LEQ
    override def add: Fun2[Int, Int, Int] = int.Add
    override def neg: Fun1[Int, Int] = int.Negate
    override def times: Fun2[Int, Int, Int] = int.Times
  }
  implicit object DoubleNumeric extends NumericBase[Double] {
    override def leq: Fun2[Double, Double, Boolean] = double.LEQ
    override def add: Fun2[Double, Double, Double] = double.Add
    override def neg: Fun1[Double, Double] = double.Negate
    override def times: Fun2[Double, Double, Double] = double.Times
  }

  implicit def exprNumeric[T: WTypeTag](implicit ev: NumericBase[T], epsilon: Epsilon[T]): Numeric[T, Expr] =
    new Numeric[T, Expr] {
      override def leq(lhs: Expr[T], rhs: Expr[T]): Expr[Boolean] =
        Computation(ev.leq, lhs, rhs)

      override def lt(lhs: Expr[T], rhs: Expr[T]): Expr[Boolean] =
        leq(add(lhs, Cst(epsilon.value)), rhs)

      override def add(lhs: Expr[T], rhs: Expr[T]): Expr[T] =
        Computation(ev.add, lhs, rhs)

      override def negate(term: Expr[T]): Expr[T] =
        Computation(ev.neg, term)

      override def times(lhs: Expr[T], rhs: Expr[T]): Expr[T] =
        Computation(ev.times, lhs, rhs)
    }

  implicit def optExprNumeric[T: WTypeTag](implicit ev: Numeric[T, Expr]): Numeric[T, SubjectTo] = {
    def condition(terms: SubjectTo[T]*): Expr[Boolean] =
      BooleanLike.exprBoolean.and(terms.map(_.condition): _*)
    new Numeric[T, SubjectTo] {
      override def leq(lhs: SubjectTo[T], rhs: SubjectTo[T]): SubjectTo[Boolean] =
        SubjectTo(ev.leq(lhs.value, rhs.value), condition(lhs, rhs))

      override def lt(lhs: SubjectTo[T], rhs: SubjectTo[T]): SubjectTo[Boolean] =
        SubjectTo(ev.lt(lhs.value, rhs.value), condition(lhs, rhs))

      override def add(lhs: SubjectTo[T], rhs: SubjectTo[T]): SubjectTo[T] =
        SubjectTo(ev.add(lhs.value, rhs.value), condition(lhs, rhs))

      override def negate(term: SubjectTo[T]): SubjectTo[T] =
        SubjectTo(ev.negate(term.value), term.condition)

      override def times(lhs: SubjectTo[T], rhs: SubjectTo[T]): SubjectTo[T] =
        SubjectTo(ev.times(lhs.value, rhs.value), condition(lhs, rhs))
    }

  }

  implicit class NumericOps[T, F[_]](val lhs: F[T])(implicit num: Numeric[T, F],
                                                    bool: BooleanLike[Boolean, F]) {
    def <=(rhs: F[T]): F[Boolean] = num.leq(lhs, rhs)
    def <(rhs: F[T]): F[Boolean] = num.lt(lhs, rhs)
    def >=(rhs: F[T]): F[Boolean] = num.geq(lhs, rhs)
    def >(rhs: F[T]): F[Boolean] = num.gt(lhs, rhs)
    def ===(rhs: F[T]): F[Boolean] = bool.and(lhs <= rhs, rhs <= lhs)
    def =!=(rhs: F[T]): F[Boolean] = bool.not(lhs === rhs)

    def *(rhs: F[T]): F[T] = num.times(lhs, rhs)
    def +(rhs: F[T]): F[T] = num.add(lhs, rhs)
    def unary_-(): F[T] = num.negate(lhs)
    def -(rhs: F[T]): F[T] = num.add(lhs, -rhs)
  }
}

trait BooleanLike[T, F[_]] {
  def and(conjuncts: F[T]*): F[T]
  def or(disjuncts: F[T]*): F[T]
  def not(term: F[T]): F[T]
}

object BooleanLike {
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

  implicit def exprBooleanT[T: WTypeTag](implicit ev: BooleanBase[T]): BooleanLike[T, Expr] =
    new BooleanLike[T, Expr] {
      override def and(conjuncts: Expr[T]*): Expr[T] = Computation(ev.and, conjuncts)
      override def or(disjuncts: Expr[T]*): Expr[T] = Computation(ev.or, disjuncts)
      override def not(term: Expr[T]): Expr[T] = Computation(ev.not, term)
    }
  implicit val exprBoolean: BooleanLike[Boolean, Expr] = exprBooleanT[Boolean]

  implicit def optExprBoolean(implicit ev: BooleanBase[Boolean],
                              ev2: BooleanLike[Boolean, Expr]): BooleanLike[Boolean, SubjectTo] =
    new BooleanLike[Boolean, SubjectTo] {
      override def and(conjuncts: SubjectTo[Boolean]*): SubjectTo[Boolean] =
        SubjectTo(
          ev2.and(conjuncts.map(_.value): _*),
          ev2.and(conjuncts.map(_.condition): _*)
        )

      override def or(disjuncts: SubjectTo[Boolean]*): SubjectTo[Boolean] =
        SubjectTo(
          ev2.or(disjuncts.map(_.value): _*),
          ev2.and(disjuncts.map(_.condition): _*)
        )

      override def not(term: SubjectTo[Boolean]): SubjectTo[Boolean] =
        SubjectTo(
          ev2.not(term.value),
          term.condition
        )
    }

  implicit class BooleanOps[T, F[_]](val lhs: F[T])(implicit bool: BooleanLike[T, F]) {
    def &&(rhs: F[T]): F[T] = bool.and(lhs, rhs)
    def ||(rhs: F[T]): F[T] = bool.or(lhs, rhs)
    def unary_~(): F[T] = bool.not(lhs)
    def implies(rhs: F[T]): F[T] = (~lhs) || rhs
    def ==>(rhs: F[T]): F[T] = lhs implies rhs
  }
}
