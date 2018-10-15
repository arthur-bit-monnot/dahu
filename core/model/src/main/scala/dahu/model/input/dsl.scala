package dahu.model.input

import dahu.core
import dahu.core.algebra
import dahu.core.algebra.{BoolLike, NumberLike, Orderable}
import dahu.model.functions._
import dahu.model.math._
import dahu.model.structs._
import dahu.model.types._
import dahu.utils.Vec

import scala.language.implicitConversions
import scala.reflect.ClassTag

object dsl {

  implicit final class Fun1Ops[I, O](private val lhs: Fun1[I, O]) extends AnyVal {
    def apply(arg: Expr[I]): Expr[O] = Computation1(lhs, arg)
  }

  implicit final class Fun2Ops[I1, I2, O](private val f: Fun2[I1, I2, O]) extends AnyVal {
    def apply(i1: Expr[I1], i2: Expr[I2]): Expr[O] =
      Computation(f, i1, i2)
  }
  implicit final class FunNOps[I1, O](private val f: FunN[I1, O]) extends AnyVal {
    def apply(i1: Expr[I1]*): Expr[O] =
      Computation(f, i1)
  }

  implicit final class LambdaOps[I, O](private val lambda: Expr[I ->: O]) extends AnyVal {
    def apply(arg: Expr[I])(implicit ev: Tag[O]): Expr[O] = Apply(lambda, arg)
  }

  implicit final class Lambda2Ops[I1, I2, O](private val lambda: Expr[I1 ->: I2 ->: O])
      extends AnyVal {
    def apply(arg1: Expr[I1], arg2: Expr[I2])(implicit to: Tag[O], ti2: Tag[I2]): Expr[O] =
      Apply(Apply(lambda, arg1), arg2)
    def partialApply(arg: Expr[I1])(implicit to: Tag[O], ti2: Tag[I2]): Expr[I2 ->: O] =
      Apply(lambda, arg)
  }

  def ITE[T](cond: Expr[Bool], t: Expr[T], f: Expr[T]): Expr[T] =
    new ITE[T](cond, t, f)

  implicit class UniversalEqualityOps[T](val lhs: Expr[T]) extends AnyVal {
    def ====(rhs: Expr[T]): Expr[Bool] = any.EQ(lhs, rhs)
  }

  implicit def double2Cst(value: Double): Expr[Double] = Cst(value)
  implicit def int2Cst(value: Int): Expr[Int] = Cst(value)

  implicit val boolLike: BoolLike[Expr[Bool]] = new BoolLike[Expr[Bool]] {
    override def and(a: Expr[Bool], b: Expr[Bool]): Expr[Bool] =
      Computation(bool.And, Seq(a, b))
    override def or(a: Expr[Bool], b: Expr[Bool]): Expr[Bool] =
      Computation(bool.Or, Seq(a, b))
    override def not(a: Expr[Bool]): Expr[Bool] =
      Computation(bool.Not, a)

    override def False: Expr[Bool] = bool.True
    override def True: Expr[Bool] = bool.False
  }

  implicit val intsNumber: NumberLike.Aux[Expr[Int], Expr[Bool], Expr[Int]] =
    Numeric.toNumberLike[Int, Expr]

  implicit val numbersDouble: NumberLike.Aux[Expr[Double], Expr[Bool], Expr[Double]] =
    Numeric.toNumberLike[Double, Expr]

  implicit def orderableIsoInt[T: TagIsoInt]: Orderable.Aux[Expr[T], Expr[Bool]] =
    new Orderable[Expr[T]] {
      override type EBool = Expr[Bool]
      override def BL: BoolLike[Expr[Bool]] = boolLike
      override def leq(a: Expr[T], b: Expr[T]): EBool =
        intsNumber.leq(TagIsoInt[T].unbox(a), TagIsoInt[T].unbox(b))
      override def lt(a: Expr[T], b: Expr[T]): EBool =
        intsNumber.lt(TagIsoInt[T].unbox(a), TagIsoInt[T].unbox(b))
    }

  implicit def toNumberLikeOps[A: NumberLike](a: A) = new algebra.NumberLike.NumberLikeOps(a)
  implicit def toOrderableLikeOps[A: Orderable](a: A) = new core.algebra.Orderable.OrderableOps(a)
  implicit def toBoolLikeOps[A: BoolLike](a: A) = new core.algebra.BoolLike.BoolLikeOps(a)

  implicit class UnboxOps[A](private val lhs: Expr[A]) extends AnyVal {
    def unboxed(implicit tag: TagIsoInt[A]): Expr[Int] =
      Computation(tag.unbox, lhs)
  }

  implicit class SequenceOps[A](private val lhs: Expr[Vec[A]]) extends AnyVal {
    def fold(monoid: Monoid[A])(implicit tag: Tag[A], ct: ClassTag[A]): Expr[A] =
      sequence.Fold(monoid).apply(lhs)
  }

  private def named[A, B](f: A => B, name: String): A => B = new Function[A, B] {
    override def apply(v1: A): B = f(v1)
    override def toString(): String = name
  }

  implicit class GeneralOps[T](private val lhs: Expr[T]) {
    private implicit def tagT: Tag[T] = lhs.typ

    def map[B](f: Fun1[T, B]): Expr[B] = Computation1(f, lhs)
    def map[B: Tag](f: T => B): Expr[B] = Computation1(Fun1.embed(f), lhs)

  }

  implicit class BooleanExprOps(a: Expr[Bool]) {
    def toDouble: Expr[Double] = ITE(a, Cst(1.0), Cst(0.0))
    @deprecated("Bool is now a subtype of Int", since = "now")
    def toInt: Expr[Int] = ITE(a, Cst(1), Cst(0))
  }

  implicit class OptionalOps[A: Tag](oa: Expr[Optional[A]]) {
    def present: Expr[Bool] = OptionalF.Present[A].apply(oa)
    def value: Expr[A] = OptionalF.Value[A].apply(oa)
    def asSequence: Expr[Vec[A]] =
      ITE[Vec[A]](present, Sequence(Vec(value)), Sequence(Vec.empty[Expr[A]]))
  }

  def collect[T: Tag]: Expr[Vec[Optional[T]]] = DynCollector(Tag[T], None)

  def map[A: Tag, B: Tag](f: Expr[A ->: B], as: Expr[Vec[A]]): Expr[Vec[B]] =
    sequence.Map[A, B].apply(f, as)

  def fold[A: Tag](f: Monoid[A], as: Expr[Vec[A]]): Expr[A] = {
    implicit def ct: ClassTag[A] = Tag[A].clazz
    sequence.Fold(f).apply(as)
  }

  def forall[Export: Tag](f: Expr[Export] => Expr[Bool]): Expr[Bool] = {
    implicit val optTag: Tag[Optional[Export]] = OptionalF.prod[Export]
    val collected: Expr[Vec[Optional[Export]]] = collect[Export]
    val of: Expr[Optional[Export] ->: Bool] =
      Lambda[Optional[Export], Bool](oe => !oe.present || f(oe.value))
    val bools: Expr[Vec[Bool]] = map(of, collected)
    fold[Bool](bool.And, bools)
  }

  def exists[Export: Tag](f: Expr[Export] => Expr[Bool]): Expr[Bool] = {
    implicit val optTag: Tag[Optional[Export]] = OptionalF.prod[Export]
    val collected: Expr[Vec[Optional[Export]]] = collect[Export]
    val of: Expr[Optional[Export] ->: Bool] =
      Lambda[Optional[Export], Bool](oe => oe.present && f(oe.value))
    val bools: Expr[Vec[Bool]] = map(of, collected)
    fold[Bool](bool.Or, bools)
  }

  def all[A: Tag]: Expr[Vec[A]] = {
    implicit val optATag: Tag[Optional[A]] = OptionalF.prod[A]
    implicit def ct: ClassTag[A] = Tag[A].clazz
    val optionals: Expr[Vec[Optional[A]]] = collect[A]
    val seqs: Expr[Vec[Vec[A]]] = map(Lambda[Optional[A], Vec[A]](oe => oe.asSequence), optionals)
    sequence.Fold(sequence.Concat[A]).apply(seqs)
  }
//    map[(Lambda(oe => oe.asSequence), collect[A])
}
