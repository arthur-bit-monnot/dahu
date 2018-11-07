package dahu.model.types

import dahu.utils.Vec

import scala.reflect.ClassTag

trait SequenceTagAny extends TagAny {
  def memberTag: TagAny
}

trait SequenceTag[T] extends Tag[Vec[T]] with SequenceTagAny {
  def memberTag: Tag[T]
}

object SequenceTag {
  def apply[T](implicit t: Tag[T], ct: ClassTag[Vec[T]]): SequenceTag[T] = SequenceTagImpl(t)

  final case class SequenceTagImplAny(memberTag: TagAny) extends SequenceTagAny {
    override def clazz: ClassTag[Vec[Any]] = implicitly[ClassTag[Vec[Any]]]
    override def typ: Tag.Type = Tag.typeOf[Vec[Any]]
    override def toString: String = s"Sequence($memberTag)"
  }
  final case class SequenceTagImpl[T](memberTag: Tag[T]) extends SequenceTag[T] {
    override def clazz: ClassTag[Vec[T]] = implicitly[ClassTag[Vec[T]]]
    override def typ: Tag.Type = Tag.typeOf[Vec[T]]
  }
}
