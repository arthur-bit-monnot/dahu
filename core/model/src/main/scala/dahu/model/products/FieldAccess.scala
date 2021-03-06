package dahu.model.products

import cats.Id
import dahu.model.functions.{Fun, Fun1, FunAny}
import dahu.model.ir.ProductF
import dahu.model.types._
import dahu.utils.Vec

trait FieldAccessAny extends FunAny {
  def prodTag: TagAny
  def fieldTag: TagAny
  def fieldPosition: Int
  def name: String
  override def toString: String = name
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

import Tag.unsafe.ofAny

case class GetField(fieldName: String) extends Fun1[Any, Any] {
  override def name: String = s"get-field($fieldName)"
  override def of(in: Any): Any =
    in match {
      case a @ ProductF(members, tpe) =>
        tpe.fields.toSeq.find(_.name == fieldName) match {
          case Some(Field(_, _, i)) => members(i)
          case None =>
            dahu.utils.errors.unexpected(s"Product object $a has no field named $fieldName")
        }
      case x => dahu.utils.errors.unexpected(s"Object $x is not a product object.")
    }
}
