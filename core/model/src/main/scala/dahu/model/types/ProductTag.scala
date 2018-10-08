package dahu.model.types

import cats.Id
import dahu.model.input.Product.PMap
import dahu.model.input.{Expr, ProductExpr}
import dahu.model.math.bool
import dahu.utils.Vec

import scala.reflect.ClassTag

// note: assumption on sealness is made in the implemenation of equals/hashCode
trait ProductTag[P[_[_]]] extends Tag[P[cats.Id]] {
  def exprProd: ProductExpr[P, Expr]
  def idProd: ProductExpr[P, cats.Id]

  override def isValid(e: Expr[P[Id]]): Expr[Boolean] = bool.True
}

object ProductTag {

  def apply[P[_[_]]](implicit instance: ProductTag[P]): ProductTag[P] = instance

  import scala.reflect.runtime.universe

  implicit def ofProd[P[_[_]]](implicit pe1: ProductExpr[P, Expr],
                               pe2: ProductExpr[P, cats.Id],
                               tt: universe.WeakTypeTag[P[cats.Id]]): ProductTag[P] =
    new ProductTagImpl[P](pe1, pe2, tt.tpe)

  private final class ProductTagImpl[P[_[_]]](override val exprProd: ProductExpr[P, Expr],
                                              override val idProd: ProductExpr[P, Id],
                                              override val typ: Tag.Type)
      extends ProductTag[P] {
    override def hashCode(): Int = typ.hashCode()
    override def equals(o: Any): Boolean = o match {
      case x: ProductTagImpl[_] => typ == x.typ
      case _                    => false
    }
    override def toString: String = s"ProductTag($typ)"
  }

  final class MapProductTag[K: ClassTag, V: ClassTag](map: Map[K, Expr[V]])(
      implicit tt: universe.WeakTypeTag[Map[K, Id[V]]])
      extends ProductTag[PMap[K, ?[_], V]] {
    type M[F[_]] = PMap[K, F, V]
    val keys: Vec[K] = Vec.fromSeq(map.keys.toSeq)
    val values: Vec[Expr[Any]] = keys.map(map(_))
    override def exprProd: ProductExpr[M, Expr] = new ProductExpr[M, Expr] {
      override def extractTerms(prod: M[Expr])(implicit ct: ClassTag[Expr[Any]]): Vec[Expr[Any]] = {
        assert(prod == map)
        values
      }

      override def buildFromTerms(terms: Vec[Expr[Any]]): M[Expr] = {
        assert(terms == values)
        map
      }
    }

    override def idProd: ProductExpr[M, Id] = new ProductExpr[M, Id] {
      override def extractTerms(prod: M[Id])(implicit ct: ClassTag[Id[Any]]): Vec[Id[Any]] = {
        assert(prod.keys == map.keys)
        keys.map(k => prod(k))
      }
      override def buildFromTerms(terms: Vec[Id[Any]]): M[Id] = {
        assert(terms.size == values.size)
        keys.zip(terms.map(_.asInstanceOf[Id[V]])).toMap
      }
    }

    override def typ: Tag.Type = tt.tpe
  }

  type Sequence[F[_], A] = Seq[F[A]]

  // TODO: remove, we have native sequences
  implicit def ofSeq[A](
      implicit tt: universe.WeakTypeTag[Seq[cats.Id[A]]]): ProductTag[Sequence[?[_], A]] =
    new ProductTag[Sequence[?[_], A]] {
      override def exprProd: ProductExpr[Sequence[?[_], A], Expr] =
        new ProductExpr[Sequence[?[_], A], Expr] {
          override def extractTerms(prod: Sequence[Expr, A])(
              implicit ct: ClassTag[Expr[Any]]): Vec[Expr[Any]] =
            Vec.fromSeq(prod.map(_.asInstanceOf[Expr[Any]]))
          override def buildFromTerms(terms: Vec[Expr[Any]]): Sequence[Expr, A] =
            terms.map(_.asInstanceOf[Expr[A]]).toSeq
        }
      override def idProd: ProductExpr[Sequence[?[_], A], Id] =
        new ProductExpr[Sequence[?[_], A], Id] {
          override def extractTerms(prod: Sequence[Id, A])(
              implicit ct: ClassTag[Id[Any]]): Vec[Id[Any]] = Vec.fromSeq(prod)
          override def buildFromTerms(terms: Vec[Id[Any]]): Sequence[Id, A] =
            terms.asInstanceOf[Vec[Id[A]]].toSeq
        }

      override def typ: Tag.Type = tt.tpe
    }
}
