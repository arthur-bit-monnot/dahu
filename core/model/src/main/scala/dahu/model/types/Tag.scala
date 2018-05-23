package dahu.model.types

import dahu.model.functions._
import dahu.utils._

import scala.reflect.runtime.universe

trait Tag[+T] {

  def typ: Tag.Type

}

object Tag extends LowPriorityTags {
  type Type = universe.Type

  def apply[T](implicit ev: Tag[T]): Tag[T] = ev

  def typeOf[T](implicit ttag: universe.WeakTypeTag[T]): universe.Type = ttag.tpe

  implicit val ofInt: Tag[Int] = TagIsoInt.ofInt
  implicit val ofBoolean: Tag[Boolean] = TagIsoInt.ofBoolean

}

trait LowPriorityTags {

  def default[T: universe.WeakTypeTag]: Tag[T] = new Tag[T] {
    override def typ: universe.Type = Tag.typeOf[T]
  }

  implicit def ofIsoInt[V: TagIsoInt]: Tag[V] = TagIsoInt[V]
  implicit def ofSequence[V: Tag]: Tag[Vec[V]] = SequenceTag[V]
  implicit def ofProduct[P[_[_]]: ProductTag]: Tag[P[cats.Id]] = ProductTag[P]
  implicit def ofFunction[I: Tag, O: Tag]: Tag[I ->: O] = LambdaTag[I, O]

  implicit val ofDouble: Tag[Double] = default[Double]
  implicit val ofString: Tag[String] = default[String]

  implicit def optionTag[T](implicit ev: universe.WeakTypeTag[Option[T]]): Tag[Option[T]] =
    new Tag[Option[T]] {
      override def typ: universe.Type = ev.tpe
    }
  implicit def eitherTag[L, R](implicit ev: universe.WeakTypeTag[Either[L, R]]): Tag[Either[L, R]] =
    new Tag[Either[L, R]] {
      override def typ: universe.Type = ev.tpe
    }

}
