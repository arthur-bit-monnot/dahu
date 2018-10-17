package dahu.model.products

import cats.Id
import dahu.model.functions.{Fun, Fun1, FunAny}
import dahu.model.types.{Tag, TagAny}

trait FieldAccessAny extends FunAny {
  def prodTag: TagAny
  def fieldTag: TagAny
  def fieldPosition: Int
  def name: String
}

abstract class FieldAccess[Data[_[_]], FieldType](implicit ev: ProductTag[Data],
                                                  ev2: Tag[FieldType])
    extends Fun1[Data[cats.Id], FieldType]
    with FieldAccessAny {
  def fieldPosition: Int

  override def of(in: Data[Id]): FieldType =
    ev.getField[FieldType](in, fieldPosition)

  override def prodTag: TagAny = ev
  override def fieldTag: TagAny = ev2
}
