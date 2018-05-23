package dahu.model.types

import dahu.model.functions.->:

trait LambdaTag[I, O] extends Tag[I ->: O]

object LambdaTag {

  def apply[I, O](implicit instance: LambdaTag[I, O]): LambdaTag[I, O] = instance

  implicit def derive[I: Tag, O: Tag]: LambdaTag[I, O] = LambdaTagImpl(Tag[I], Tag[O])

  final case class LambdaTagImpl[I, O](it: Tag[I], ot: Tag[O]) extends LambdaTag[I, O] {
    override def typ: Tag.Type = ??? // TODO
  }

}
