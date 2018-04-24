package dahu.model.types

import cats.Id
import dahu.IArray
import dahu.model.input.{ProductExpr, Tentative}

import scala.annotation.switch
import scala.reflect.ClassTag

trait Tag[+T] {

  def typ: Tag.Type

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
trait BoxedInt[T] extends TagIsoInt[T]

trait ProductTag[P[_[_]]] extends Tag[P[cats.Id]] {
  def exprProd: ProductExpr[P, Tentative]
  def idProd: ProductExpr[P, cats.Id]
}
object ProductTag {

  import scala.reflect.runtime.universe

  implicit def ofProd[P[_[_]]](implicit pe1: ProductExpr[P, Tentative],
                               pe2: ProductExpr[P, cats.Id],
                               tt: universe.WeakTypeTag[P[cats.Id]]): ProductTag[P] =
    new ProductTag[P] {

      override def exprProd: ProductExpr[P, Tentative] = pe1
      override def idProd: ProductExpr[P, Id] = pe2

      override def typ: Tag.Type = tt.tpe
    }

  type Sequence[F[_], A] = Seq[F[A]]

  implicit def ofSeq[A](
      implicit tt: universe.WeakTypeTag[Seq[cats.Id[A]]]): ProductTag[Sequence[?[_], A]] =
    new ProductTag[Sequence[?[_], A]] {
      override def exprProd: ProductExpr[Sequence[?[_], A], Tentative] =
        new ProductExpr[Sequence[?[_], A], Tentative] {
          override def extractTerms(prod: Sequence[Tentative, A])(
              implicit ct: ClassTag[Tentative[Any]]): IArray[Tentative[Any]] =
            IArray.fromSeq(prod.map(_.asInstanceOf[Tentative[Any]]))
          override def buildFromTerms(terms: IArray[Tentative[Any]]): Sequence[Tentative, A] =
            terms.map(_.asInstanceOf[Tentative[A]]).toSeq
        }
      override def idProd: ProductExpr[Sequence[?[_], A], Id] =
        new ProductExpr[Sequence[?[_], A], Id] {
          override def extractTerms(prod: Sequence[Id, A])(
              implicit ct: ClassTag[Id[Any]]): IArray[Id[Any]] = IArray.fromSeq(prod)
          override def buildFromTerms(terms: IArray[Id[Any]]): Sequence[Id, A] =
            terms.asInstanceOf[IArray[Id[A]]].toSeq
        }

      override def typ: Tag.Type = tt.tpe
    }
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
