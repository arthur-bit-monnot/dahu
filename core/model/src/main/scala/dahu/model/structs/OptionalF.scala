package dahu.model.structs
import cats.Id
import dahu.model.input.{Expr, ProductExpr}
import dahu.model.products.FieldAccess
import dahu.model.types.{Bool, ProductTag, Tag}
import dahu.utils.ClassTag

final case class OptionalF[F[_], T](value: F[T], presence: F[Bool])

object OptionalF {
  type Optional[T] = OptionalF[Id, T]

  def productTag[T: Tag]: Tag[Optional[T]] = new ProductTag[OptionalF[?[_], T]] {
    override val clazz: ClassTag[OptionalF[cats.Id, T]] =
      implicitly[ClassTag[OptionalF[cats.Id, T]]]
    override def exprProd: ProductExpr[OptionalF[?[_], T], Expr] = ???
    override def idProd: ProductExpr[OptionalF[?[_], T], Id] = ???
    override def typ: Tag.Type = ???
  }

  // TODO: product tags should be easily extractable (by name) from the product tag
  def Value[T]: FieldAccess[OptionalF[?[_], T], T] = ???
  def Present[T]: FieldAccess[OptionalF[?[_], T], Bool] = ???
}
