package dahu.model.types

import dahu.model.functions._
import dahu.model.products.ProductTag
import dahu.model.types
import dahu.model.types.LambdaTag.LambdaTagImpl
import dahu.utils._

import scala.reflect.runtime.universe

trait TagAny {

  def typ: Tag.Type
  def clazz: ClassTag[_]

  def isInt: Boolean = this match {
    case _: RawInt => !isBoolean
    case _         => false
  }
  def isBoolean: Boolean = this == Tag.ofBoolean

  def intersects(t: TagAny): Boolean = typ == t.typ

}

trait Tag[T] extends TagAny {
  override def clazz: ClassTag[T]
}

object Tag extends LowPriorityTags {
  type Type = universe.Type

  def apply[T](implicit ev: Tag[T]): Tag[T] = ev

  def typeOf[T](implicit ttag: universe.WeakTypeTag[T]): universe.Type = ttag.tpe

  implicit val ofInt: Tag[Int] = TagIsoInt.ofInt
  implicit val ofType: Tag[TagAny] = Tag.default[TagAny]

  object unsafe {

    implicit val ofAny = default[Any]

  }

}

trait LowPriorityTags extends VeryLowPriorityTags {
  private case class DefaultTag[T: ClassTag](typ: universe.Type) extends Tag[T] {
    override def toString: String = typ.toString
    override def clazz: ClassTag[T] = implicitly[ClassTag[T]]
  }

  def default[T: universe.WeakTypeTag: ClassTag]: Tag[T] = DefaultTag(Tag.typeOf[T])

  implicit def ofSequence[V: Tag]: Tag[Vec[V]] = SequenceTag[V]
  implicit def ofFunction[I: Tag, O: Tag]: Tag[I ->: O] = LambdaTag[I, O]
  implicit def ofFunction2[I1: Tag, I2: Tag, O: Tag]: Tag[I1 ->: I2 ->: O] =
    LambdaTagImpl(Tag[I1], ofFunction[I2, O])

  implicit def ofFunction3[I1: Tag, I2: Tag, I3: Tag, O: Tag]: Tag[I1 ->: I2 ->: I3 ->: O] =
    LambdaTagImpl(Tag[I1], ofFunction2[I2, I3, O])

  implicit val ofDouble: Tag[Double] = default[Double]
  implicit val ofString: Tag[String] = default[String]
}

trait VeryLowPriorityTags {
  implicit val ofBoolean: Tag[Bool] = BoolTag
  implicit def ofIsoInt[V: TagIsoInt]: Tag[V] = TagIsoInt[V]
}
