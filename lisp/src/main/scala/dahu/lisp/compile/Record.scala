package dahu.lisp.compile
import dahu.model.ir.ProductF
import dahu.model.products.ProductTagAny
import dahu.model.types._
import dahu.utils._

case class RecordType(name: String, fields: Seq[(Type, String)]) extends ProductTagAny {

  override def fromValues(fields: Vec[Any]): Any = ProductF[Any](fields, this)
  override def buildFromValues(fields: Vec[Value]): Any = ProductF[Value](fields, this)

  override def typ: Tag.Type = ???
  override def clazz: ClassTag[_] = implicitly[ClassTag[ProductF[Any]]]
}
