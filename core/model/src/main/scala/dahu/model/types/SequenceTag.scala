package dahu.model.types

import dahu.utils.Vec

import scala.reflect.ClassTag

trait SequenceTag[T] extends Tag[Vec[T]]

object SequenceTag {
  def apply[T](implicit t: Tag[T], ct: ClassTag[Vec[T]]): SequenceTag[T] = SequenceTagImpl(t)

  final case class SequenceTagImpl[T](memberTag: Tag[T])(implicit ct: ClassTag[Vec[T]])
      extends SequenceTag[T] {
    override def clazz: ClassTag[Vec[T]] = implicitly[ClassTag[Vec[T]]]
    override def typ: Tag.Type = Tag.typeOf[Vec[T]]
  }
}
