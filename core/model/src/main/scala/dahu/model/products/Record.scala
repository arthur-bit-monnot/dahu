package dahu.model.products

import dahu.model.functions.{Fun2, FunAny}
import dahu.model.input.Expr
import dahu.model.ir.{ComputationF, ProductF}
import dahu.model.products.{Field, ProductTagAny}
import dahu.model.types.LambdaTag.LambdaTagImpl
import dahu.model.types._
import dahu.utils._

case class RecordType(name: String, fields: Vec[Field]) extends ProductTagAny {

  override def getField(i: Int, product: Any): Any = product.asInstanceOf[ProductF[Any]].members(i)
  override def fromValues(fields: Vec[Any]): Any = ProductF[Any](fields, this)
  override def buildFromValues(fields: Vec[Value]): Any = ProductF[Value](fields, this)

  override def typ: Tag.Type = ???
  override def clazz: ClassTag[_] = implicitly[ClassTag[ProductF[Any]]]

  override def toString: String = name
}
object RecordType {
  def apply(name: String, _fields: (Type, String)*): RecordType = {
    val fields = _fields.zipWithIndex.map {
      case ((tpe, name), index) => Field(name, tpe, index)
    }.toVec
    RecordType(name, fields)
  }
}

import Tag.unsafe.ofAny

case class Constructor(tpe: RecordType) extends FunAny {
  override def arity: Option[Int] = None
  override def compute(args: Vec[Value]): ProductF[Value] = {
    ProductF[Value](args, tpe)
  }
  override def name: String = tpe.name + "."
  override def outType: RecordType = tpe

  override def funType: LambdaTagAny =
    LambdaTag.of(SequenceTag[Any], tpe)
}
