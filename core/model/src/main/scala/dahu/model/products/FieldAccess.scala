package dahu.model.products

import cats.Id
import dahu.model.functions.Fun1
import dahu.model.types.{ProductTag, Tag}

abstract class FieldAccess[Data[_[_]], FieldType](implicit ev: ProductTag[Data],
                                                  ev2: Tag[FieldType])
    extends Fun1[Data[cats.Id], FieldType] {
  def fieldPosition: Int

  override def of(in: Data[Id]): FieldType =
    ev.idProd.extractTerms(in).apply(fieldPosition).asInstanceOf[FieldType]
}
object FieldAccess {

  def apply[Data[_[_]], FieldType](_name: String, _fieldPosition: Int)(
      implicit ev: ProductTag[Data],
      ev2: Tag[FieldType]): FieldAccess[Data, FieldType] = new FieldAccess[Data, FieldType]() {
    override def fieldPosition: Int = _fieldPosition
    override def name: String = _name
  }

}
