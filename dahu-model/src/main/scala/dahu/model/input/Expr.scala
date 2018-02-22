package dahu.model.input

import cats.Id
import dahu.model.functions._
import dahu.model.types._

final case class ConstraintViolated(constraint: Expr[Boolean])

sealed abstract class OptionExpr[T] {
  def typ: Tag[T]
}

final case class SubjectTo[T](value: Expr[T], condition: Expr[Boolean])
    extends OptionExpr[Either[ConstraintViolated, T]] {
  override def typ: Tag[Either[ConstraintViolated, T]] =
    ??? // TODO: we should be able to construct it from the value's tag
}

sealed abstract class Expr[T] extends OptionExpr[T]

final case class Input[T: Tag](name: String) extends Expr[T] {
  override def typ: Tag[T] = Tag[T]
  def bind(value: T): Bind[T] = Bind(this, value)
}

final case class Cst[T: Tag](value: T) extends Expr[T] {
  override def typ: Tag[T] = Tag[T]
}

sealed abstract class Computation[O] extends Expr[O] {
  override def typ: Tag[O] = f.outType
  def f: Fun[O]
  def args: Seq[Expr[Any]]
}

final case class Product[T[_[_]], V <: T[Expr]](value: V)(implicit tt: Tag[T[Id]],
                                                          pe1: ProductExpr[T, Expr],
                                                          pe2: ProductExpr[T, Id])
    extends Expr[T[Id]] {
  override def typ: Tag[T[Id]] = ??? // TODO
  def members: Seq[Expr[Any]] = pe1.extractTerms(value)
  def buildFromVals(terms: Seq[Any]): T[Id] = pe2.buildFromTerms(terms)
  def buildFromExpr(terms: Seq[Expr[Any]]): T[Expr] = pe1.buildFromTerms(terms)

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
  def apply[I, O](f: Fun1[I, O], in: Expr[I]): Computation1[I, O] =
    Computation1(f, in)

  def apply[I1, I2, O](f: Fun2[I1, I2, O], in1: Expr[I1], in2: Expr[I2]): Computation2[I1, I2, O] =
    Computation2(f, in1, in2)

  def apply[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                           in1: Expr[I1],
                           in2: Expr[I2],
                           in3: Expr[I3]): Computation3[I1, I2, I3, O] =
    Computation3(f, in1, in2, in3)

  def apply[I, O](fun: FunN[I, O], arguments: Seq[Expr[I]]): Computation[O] =
    new Computation[O] {
      override def f: Fun[O] = fun
      override def args: Seq[Expr[Any]] = arguments.asInstanceOf[Seq[Expr[Any]]]
    }
}
final case class Computation1[I, O](f: Fun1[I, O], in: Expr[I]) extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in).asInstanceOf[Seq[Expr[Any]]]
}

final case class Computation2[I1, I2, O](f: Fun2[I1, I2, O], in: Expr[I1], in2: Expr[I2])
    extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in, in2).asInstanceOf[Seq[Expr[Any]]]
}

final case class Computation3[I1, I2, I3, O](f: Fun3[I1, I2, I3, O],
                                             in: Expr[I1],
                                             in2: Expr[I2],
                                             in3: Expr[I3])
    extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in, in2, in3).asInstanceOf[Seq[Expr[Any]]]
}

final case class Computation4[I1, I2, I3, I4, O](f: Fun4[I1, I2, I3, I4, O],
                                                 in: Expr[I1],
                                                 in2: Expr[I2],
                                                 in3: Expr[I3],
                                                 in4: Expr[I4])
    extends Computation[O] {
  override val args: Seq[Expr[Any]] = Seq(in, in2, in3, in4).toSeq.asInstanceOf[Seq[Expr[Any]]]
}
