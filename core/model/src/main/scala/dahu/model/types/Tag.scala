package dahu.model.types

import dahu.model.functions._
import dahu.model.input.Expr
import dahu.model.math.bool
import dahu.utils._

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe

trait TagAny {

  def typ: Tag.Type

  def isInt: Boolean = this match {
    case _: RawInt => !isBoolean
    case _         => false
  }
  def isBoolean: Boolean = this == Tag.ofBoolean

  def intersects(t: TagAny): Boolean = typ == t.typ

}

trait Tag[T] extends TagAny

object Tag extends LowPriorityTags {
  type Type = universe.Type

  def apply[T](implicit ev: Tag[T]): Tag[T] = ev

  def typeOf[T](implicit ttag: universe.WeakTypeTag[T]): universe.Type = ttag.tpe

  implicit val ofInt: Tag[Int] = TagIsoInt.ofInt

}

trait LowPriorityTags extends VeryLowPriorityTags {
  import Bool._
  private case class DefaultTag[T](typ: universe.Type) extends Tag[T]

  def default[T: universe.WeakTypeTag]: Tag[T] = DefaultTag(Tag.typeOf[T])

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

trait VeryLowPriorityTags {
  implicit val ofBoolean: Tag[Bool] = BoolTag
}
