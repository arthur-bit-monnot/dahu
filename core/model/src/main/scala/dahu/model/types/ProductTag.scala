package dahu.model.types

import cats.Id
import dahu.model.input.{Expr, ProductExpr}
import dahu.utils.Vec

import scala.reflect.ClassTag

trait ProductTag[P[_[_]]] extends Tag[P[cats.Id]] {
  def exprProd: ProductExpr[P, Expr]
  def idProd: ProductExpr[P, cats.Id]
}

object ProductTag {

  def apply[P[_[_]]](implicit instance: ProductTag[P]): ProductTag[P] = instance

  import scala.reflect.runtime.universe

  implicit def ofProd[P[_[_]]](implicit pe1: ProductExpr[P, Expr],
                               pe2: ProductExpr[P, cats.Id],
                               tt: universe.WeakTypeTag[P[cats.Id]]): ProductTag[P] =
    new ProductTag[P] {

      override def exprProd: ProductExpr[P, Expr] = pe1
      override def idProd: ProductExpr[P, Id] = pe2

      override def typ: Tag.Type = tt.tpe
    }

  type Sequence[F[_], A] = Seq[F[A]]

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
