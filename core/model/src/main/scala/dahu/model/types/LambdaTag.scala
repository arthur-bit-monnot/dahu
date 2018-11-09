package dahu.model.types

import dahu.model.functions.->:
import dahu.model.input.Expr
import dahu.model.math.bool
import dahu.utils.ClassTag

trait LambdaTagAny extends TagAny {
  def inType: TagAny
  def outType: TagAny
}

trait LambdaTag[I, O] extends Tag[I ->: O] with LambdaTagAny

object LambdaTag {

  def apply[I, O](implicit instance: LambdaTag[I, O]): LambdaTag[I, O] = instance

  implicit def derive[I: Tag, O: Tag]: LambdaTag[I, O] = LambdaTagImpl(Tag[I], Tag[O])

  def of(in: TagAny, out: TagAny): LambdaTag[_, _] = new LambdaTag[Any, Any] {
    override def inType: Type = in
    override def outType: Type = out
    override def clazz: ClassTag[Any ->: Any] = implicitly[ClassTag[Any ->: Any]]
    override def typ: Tag.Type = ???
  }
  def of(in1: TagAny, in2: TagAny, out: TagAny): LambdaTag[_, _] =
    of(in1, of(in2, out))

  final case class LambdaTagImpl[I, O](it: Tag[I], ot: Tag[O]) extends LambdaTag[I, O] {
    override def clazz: ClassTag[I ->: O] = implicitly[ClassTag[I ->: O]]
    override def typ: Tag.Type = ??? // TODO

    override def inType: Type = it
    override def outType: Type = ot
  }

}
