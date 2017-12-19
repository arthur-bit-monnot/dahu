package dahu.ast

import dahu.arrows.{==>, OpaqueIntSubset, TypeInstances}
import dahu.recursion.{ExprF, InputF}

sealed abstract class IndexLabelImpl {
  type T
  def apply(s: Int): T
  def unwrap(lbl: T): Int
  def subst[F[_]](fs: F[Int]): F[T]
  def unsubst[F[_]](fs: F[T]): F[Int]
}

trait ASTable {

  /** Opaque type representing the IDs of the expression in this table. */
  val EId: IndexLabelImpl = new IndexLabelImpl {
    type T = Int
    override def apply(s: Int): T                = s
    override def unwrap(lbl: T): Int             = lbl
    override def subst[F[_]](fs: F[Int]): F[T]   = fs
    override def unsubst[F[_]](fs: F[T]): F[Int] = fs
  }
  type EId = this.EId.T

  type Expr     = ExprF[EId]
  type Variable = InputF[EId]

  def root: EId
  def arrow: EId ==> Expr

  implicit def ids: OpaqueIntSubset[EId]
}
