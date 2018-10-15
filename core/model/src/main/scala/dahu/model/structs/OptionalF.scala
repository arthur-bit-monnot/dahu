package dahu.model.structs
import cats.Id
import dahu.model.input.Expr
import dahu.model.products.{Field, FieldAccess, GenConstructor, ProductTag}
import dahu.model.types.Tag.Type
import dahu.model.types.{Bool, Tag, TagAny, Value}
import dahu.utils.{ClassTag, Vec}

final case class OptionalF[F[_], T](value: F[T], presence: F[Bool]) {
  def this(arr: Seq[F[Any]]) = this(arr(0).asInstanceOf[F[T]], arr(1).asInstanceOf[F[Bool]])
}

object OptionalF {
  type Optional[T] = OptionalF[Id, T]

//  implicitly[GenConstructor[OptionalF[?[_], Int]]]

  // TODO: change name
  def prod[T: Tag] = new ProductTag[OptionalF[?[_], T]] {
    override val fields: Vec[Field] = Vec(
      Field("value", Tag[T], 0),
      Field("presence", Tag.ofBoolean, 1)
    )
    override def clazz: ClassTag[OptionalF[Id, T]] = implicitly[ClassTag[OptionalF[Id, T]]]
    override def typ: Type = ???
    override def fromValues(fields: Vec[Any]) = {
      new OptionalF[Id, T](fields.toSeq)
    }
    override def getFields(prod: OptionalF[Expr, T]): Vec[Expr[Any]] =
      Vec[Expr[Any]](prod.value, prod.presence)
    override def getFieldsIdentity(prod: OptionalF[Id, T]): Vec[Any] =
      Vec[Any](prod.value, prod.presence)
  }

  def Value[T: Tag]: FieldAccess[OptionalF[?[_], T], T] = prod[T].getAccessor("value")
  def Present[T: Tag]: FieldAccess[OptionalF[?[_], T], Bool] = prod[T].getAccessor("presence")
}
