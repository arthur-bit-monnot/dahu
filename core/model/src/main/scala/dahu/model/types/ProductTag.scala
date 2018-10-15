package dahu.model.types

import cats.Id
import dahu.model.input.{Expr, ProductExpr}
import dahu.model.math.bool
import dahu.utils.Vec

import scala.reflect.ClassTag

// note: assumption on sealness is made in the implemenation of equals/hashCode
trait ProductTag[P[_[_]]] extends Tag[P[cats.Id]] {
  def exprProd: ProductExpr[P, Expr]
  def idProd: ProductExpr[P, cats.Id]
}

object ProductTag {

  def apply[P[_[_]]](implicit instance: ProductTag[P]): ProductTag[P] = instance

  import scala.reflect.runtime.universe

  implicit def ofProd[P[_[_]]](implicit pe1: ProductExpr[P, Expr],
                               pe2: ProductExpr[P, Id],
                               tt: universe.WeakTypeTag[P[Id]],
                               ct: ClassTag[P[Id]]): ProductTag[P] =
    new ProductTagImpl[P](pe1, pe2, tt.tpe, ct)

  private final class ProductTagImpl[P[_[_]]](override val exprProd: ProductExpr[P, Expr],
                                              override val idProd: ProductExpr[P, Id],
                                              override val typ: Tag.Type,
                                              override val clazz: ClassTag[P[Id]])
      extends ProductTag[P] {
    override def hashCode(): Int = typ.hashCode()
    override def equals(o: Any): Boolean = o match {
      case x: ProductTagImpl[_] => typ == x.typ
      case _                    => false
    }
    override def toString: String = s"ProductTag($typ)"
  }
}
