package dahu.lisp.compile
import dahu.model.functions.{Fun2, FunAny}
import dahu.model.input.Expr
import dahu.model.ir.{ComputationF, ProductF}
import dahu.model.products.{Field, ProductTagAny}
import dahu.model.types._
import dahu.utils._

case class RecordType(name: String, fields: Vec[Field]) extends ProductTagAny {

  override def fromValues(fields: Vec[Any]): Any = ProductF[Any](fields, this)
  override def buildFromValues(fields: Vec[Value]): Any = ProductF[Value](fields, this)

  override def typ: Tag.Type = ???
  override def clazz: ClassTag[_] = implicitly[ClassTag[ProductF[Any]]]

  override def toString: LispStr = name
}
object RecordType {
  def apply(name: String, _fields: Seq[(Type, String)]): RecordType = {
    val fields = _fields.zipWithIndex.map {
      case ((tpe, name), index) => Field(name, tpe, index)
    }.toVec
    RecordType(name, fields)
  }
}

case class GetField(fieldName: String) extends FunAny {
  override def compute(args: Vec[Value]): Any = {
    require(args.size == 1)
    args(0) match {
      case a @ ProductF(members, tpe) =>
        tpe.fields.toSeq.find(_.name == fieldName) match {
          case Some(Field(_, _, i)) => members(i)
          case None =>
            dahu.utils.errors.unexpected(s"Product object $a has no field named $fieldName")
        }
      case x => dahu.utils.errors.unexpected(s"Object $x is not a product object.")
    }

  }
  override def name: String = s"get-field($fieldName)"
  override def outType: Type = Tag.unsafe.ofAny
}
