package dahu.model.input

import cats.Id
import dahu.model.functions._
import dahu.model.types._

final case class ConstraintViolated(constraint: Tentative[Boolean])

/** Evaluation yields an Either[ConstraintViolated, T] */
sealed trait Tentative[T] {

  def typ: Tag[T]

  def subjectTo(cond: Tentative[T] => Tentative[Boolean]): Tentative[T] =
    SubjectTo(this, cond(this))
}

/** Evaluation: eval(condition).flatMap(eval(value)) */
final case class SubjectTo[T](value: Tentative[T], condition: Tentative[Boolean])
    extends Tentative[T] {
  override def typ: Tag[T] = value.typ
}

sealed abstract class Term[T] extends Tentative[T]

/** Evaluation yields a Right[T] */
final case class Input[T: Tag](name: String) extends Term[T] {
  override def typ: Tag[T] = Tag[T]
  def bind(value: T): Bind[T] = Bind(this, value)
}

/** Evaluation returns a Right[T](value) */
final case class Cst[T: Tag](value: T) extends Term[T] {
  override def typ: Tag[T] = Tag[T]
}

sealed abstract class Computation[O] extends Tentative[O] {
  override def typ: Tag[O] = f.outType
  def f: Fun[O]
  def args: Seq[Tentative[Any]]

  override def toString: String = s"$f(${args.mkString(", ")})"
}

final case class Product[T[_[_]], V <: T[Tentative]](value: V)(implicit tt: Tag[T[Id]],
                                                               pe1: ProductExpr[T, Tentative],
                                                               pe2: ProductExpr[T, Id])
    extends Tentative[T[Id]] {
  override def typ: Tag[T[Id]] = ??? // TODO
  def members: Seq[Tentative[Any]] = pe1.extractTerms(value)
  def buildFromVals(terms: Seq[Any]): T[Id] = pe2.buildFromTerms(terms)
  def buildFromExpr(terms: Seq[Tentative[Any]]): T[Tentative] = pe1.buildFromTerms(terms)

}

trait ProductExpr[P[_[_]], F[_]] {

  def extractTerms(prod: P[F]): Seq[F[Any]]
  def buildFromTerms(terms: Seq[F[Any]]): P[F]

}
object ProductExpr {

  def apply[P[_[_]], F[_]](implicit instance: ProductExpr[P, F]): ProductExpr[P, F] = instance

  import shapeless._

  trait HListExtract[H <: HList, F[_]] {
    def terms(h: H): List[F[Any]]
    def fromTerms(l: Seq[F[Any]]): H
  }

  implicit def genPE[P[_[_]], F[_], H <: HList](implicit gen: Generic.Aux[P[F], H],
                                                hListExtract: HListExtract[H, F]) =
    new ProductExpr[P, F] {
      override def extractTerms(prod: P[F]): Seq[F[Any]] = hListExtract.terms(gen.to(prod))
      override def buildFromTerms(terms: Seq[F[Any]]): P[F] =
        gen.from(hListExtract.fromTerms(terms))
    }

  implicit def peOfHNil[F[_]] = new HListExtract[HNil, F] {
    override def terms(h: HNil): List[F[Any]] = Nil
    override def fromTerms(l: Seq[F[Any]]): HNil = {
      require(l.isEmpty)
      HNil
    }
  }
  implicit def peOfHlist[H, T <: HList, F[_]](implicit t: HListExtract[T, F]) =
    new HListExtract[F[H] :: T, F] {
      override def terms(l: F[H] :: T): List[F[Any]] =
        l.head.asInstanceOf[F[Any]] :: t.terms(l.tail)
      override def fromTerms(l: Seq[F[Any]]): F[H] :: T =
        l.head.asInstanceOf[F[H]] :: t.fromTerms(l.tail)
    }
}

final case class Bind[T](variable: Input[T], value: T)

object Computation {
  def apply[I, O](f: Fun1[I, O], in: Tentative[I]): Computation1[I, O] =
    Computation1(f, in)

  def apply[I1, I2, O](f: Fun2[I1, I2, O],
                       in1: Tentative[I1],
                       in2: Tentative[I2]): Computation2[I1, I2, O] =
    Computation2(f, in1, in2)

  def apply[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                           in1: Tentative[I1],
                           in2: Tentative[I2],
                           in3: Tentative[I3]): Computation3[I1, I2, I3, O] =
    Computation3(f, in1, in2, in3)

  def apply[I, O](fun: FunN[I, O], arguments: Seq[Tentative[I]]): Computation[O] =
    new Computation[O] {
      override def f: Fun[O] = fun
      override def args: Seq[Tentative[Any]] = arguments.asInstanceOf[Seq[Tentative[Any]]]
    }
}
final case class Computation1[I, O](f: Fun1[I, O], in: Tentative[I]) extends Computation[O] {
  override val args: Seq[Tentative[Any]] = Seq(in).asInstanceOf[Seq[Tentative[Any]]]
}

final case class Computation2[I1, I2, O](f: Fun2[I1, I2, O], in: Tentative[I1], in2: Tentative[I2])
    extends Computation[O] {
  override val args: Seq[Tentative[Any]] = Seq(in, in2).asInstanceOf[Seq[Tentative[Any]]]
}

final case class Computation3[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                                             in: Tentative[I1],
                                             in2: Tentative[I2],
                                             in3: Tentative[I3])
    extends Computation[O] {
  override val args: Seq[Tentative[Any]] = Seq(in, in2, in3).asInstanceOf[Seq[Tentative[Any]]]
}

final case class Computation4[I1, I2, I3, I4, O](f: Fun4[I1, I2, I3, I4, O],
                                                 in: Tentative[I1],
                                                 in2: Tentative[I2],
                                                 in3: Tentative[I3],
                                                 in4: Tentative[I4])
    extends Computation[O] {
  override val args: Seq[Tentative[Any]] = Seq(in, in2, in3, in4).asInstanceOf[Seq[Tentative[Any]]]
}
