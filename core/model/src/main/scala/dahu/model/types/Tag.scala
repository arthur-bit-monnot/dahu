package dahu.model.types

import cats.Id
import dahu.model.functions._
import dahu.utils._
import dahu.model.input.{Expr, ProductExpr}

import scala.annotation.switch
import scala.reflect.ClassTag

trait Tag[+T] {

  def typ: Tag.Type

}

object Tag {
  import scala.reflect.runtime.universe
  type Type = universe.Type

  def apply[T](implicit ev: Tag[T]): Tag[T] = ev

  def typeOf[T](implicit ttag: universe.WeakTypeTag[T]): universe.Type = ttag.tpe

  implicit case object ofInt extends BoxedInt[Int] {
    override val typ: Type = typeOf[Int]
    override def fromInt(i: Int): Int = i
    override def toInt(t: Int): Int = t

    override val min: Int = Integer.MIN_VALUE / 2 + 1
    override val max: Int = Integer.MAX_VALUE / 2 - 1
  }

  implicit case object ofBoolean extends TagIsoInt[Boolean] {
    override def typ: Type = typeOf[Boolean]
    override def toInt(t: Boolean): Int = if(t) 1 else 0
    def fromInt(i: Int): Boolean = (i: @switch) match {
      case 0 => false
      case 1 => true
      case _ => ???
    }

    override val min: Int = 0
    override val max: Int = 1
  }

  final case class LambdaTag[I, O](it: Tag[I], ot: Tag[O]) extends Tag[I -> O] {
    override def typ: Tag.Type = ??? // TODO
  }

  implicit def ofFunction[I: Tag, O: Tag]: Tag[I -> O] = ofLambda[I, O]
  implicit def ofLambda[I: Tag, O: Tag]: LambdaTag[I, O] = LambdaTag(Tag[I], Tag[O])

  def tagInstance[T](implicit ttag: universe.WeakTypeTag[T]): Tag[T] = new Tag[T] {
    override def typ: Type = ttag.tpe
  }
  implicit val ofDouble = tagInstance[Double]
  implicit val ofString = tagInstance[String]

  implicit def optionTag[T](implicit ev: universe.WeakTypeTag[Option[T]]): Tag[Option[T]] =
    new Tag[Option[T]] {
      override def typ: Type = ev.tpe
    }
  implicit def eitherTag[L, R](implicit ev: universe.WeakTypeTag[Either[L, R]]): Tag[Either[L, R]] =
    new Tag[Either[L, R]] {
      override def typ: Type = ev.tpe
    }

  def default[T: universe.WeakTypeTag]: Tag[T] = new Tag[T] {
    override def typ: Type = typeOf[T]
  }
}

/** A type for which an isomophism to a subset of integers is known. */
trait TagIsoInt[T] extends Tag[T] {

  def fromInt(i: Int): T
  def toValue(i: Int): Value = Value(fromInt(i))
  def toInt(t: T): Int
  def toIntUnsafe(t: Any): Int = toInt(t.asInstanceOf[T])

  val min: Int
  val max: Int
  def numInstances: Int = max - min + 1

  private implicit def selfTag: TagIsoInt[T] = this

  val unbox: Unbox[T] = new Unbox[T]()(this)
  val box: Box[T] = unbox.reverse
}

object TagIsoInt {

  def apply[T](implicit ev: TagIsoInt[T]): TagIsoInt[T] = ev

  import scala.reflect.runtime.universe
  def fromEnum[T: universe.WeakTypeTag](values: Seq[T]): TagIsoInt[T] = new TagIsoInt[T] {
    override def toInt(t: T): Int = values.indexOf(t)
    override def fromInt(i: Int): T = values(i)

    override val min: Int = 0
    override val max: Int = values.size - 1

    override def typ: Tag.Type = Tag.typeOf[T]

    assert(numInstances == max - min + 1)
  }
}

/** Marker trait for a type that is a simple container of Int. */
trait BoxedInt[T] extends TagIsoInt[T] {}

trait ProductTag[P[_[_]]] extends Tag[P[cats.Id]] {
  def exprProd: ProductExpr[P, Expr]
  def idProd: ProductExpr[P, cats.Id]
}
object ProductTag {

  import scala.reflect.runtime.universe

  implicit def ofProd[P[_[_]]](implicit pe1: ProductExpr[P, Expr],
                               pe2: ProductExpr[P, cats.Id],
                               tt: universe.WeakTypeTag[P[cats.Id]]): ProductTag[P] =
    new ProductTag[P] {

      override def exprProd: ProductExpr[P, Expr] = pe1
      override def idProd: ProductExpr[P, Id] = pe2

      override def typ: Tag.Type = tt.tpe
    }

  type Sequence[F[_], A] = Seq[F[A]]

  implicit def ofSeq[A](
      implicit tt: universe.WeakTypeTag[Seq[cats.Id[A]]]): ProductTag[Sequence[?[_], A]] =
    new ProductTag[Sequence[?[_], A]] {
      override def exprProd: ProductExpr[Sequence[?[_], A], Expr] =
        new ProductExpr[Sequence[?[_], A], Expr] {
          override def extractTerms(prod: Sequence[Expr, A])(
              implicit ct: ClassTag[Expr[Any]]): Vec[Expr[Any]] =
            Vec.fromSeq(prod.map(_.asInstanceOf[Expr[Any]]))
          override def buildFromTerms(terms: Vec[Expr[Any]]): Sequence[Expr, A] =
            terms.map(_.asInstanceOf[Expr[A]]).toSeq
        }
      override def idProd: ProductExpr[Sequence[?[_], A], Id] =
        new ProductExpr[Sequence[?[_], A], Id] {
          override def extractTerms(prod: Sequence[Id, A])(
              implicit ct: ClassTag[Id[Any]]): Vec[Id[Any]] = Vec.fromSeq(prod)
          override def buildFromTerms(terms: Vec[Id[Any]]): Sequence[Id, A] =
            terms.asInstanceOf[Vec[Id[A]]].toSeq
        }

      override def typ: Tag.Type = tt.tpe
    }
}
