package dahu.ast

import dahu.arrows.{==>, OpaqueIntSubset}
import dahu.ast.Ops.Algebra
import dahu.recursion.{ExprF, InputF}

import scala.reflect.ClassTag

sealed abstract class IndexLabelImpl {
  type T
  def apply(s: Int): T = fromInt(s)

  def fromInt(s: Int): T
  def toInt(lbl: T): Int
  def fromIntF[F[_]](fs: F[Int]): F[T]
  def toIntF[F[_]](fs: F[T]): F[Int]
}

sealed abstract class SubIndexLabelImpl[UB] extends IndexLabelImpl {
  def wrap(s: UB): T
  def unwrap(lbl: T): UB
  def subst[F[_]](fs: F[UB]): F[T]
  def unsubst[F[_]](fs: F[T]): F[UB]
}

trait ASTable {

  /** Opaque type representing the IDs of the expression in this table. */
  val EId: IndexLabelImpl = new IndexLabelImpl {
    type T = Int
    override def fromInt(s: Int): T               = s
    override def toInt(lbl: T): Int               = lbl
    override def fromIntF[F[_]](fs: F[Int]): F[T] = fs
    override def toIntF[F[_]](fs: F[T]): F[Int]   = fs
  }
  type EId = this.EId.T

  val VarId: SubIndexLabelImpl[EId] = new SubIndexLabelImpl[EId] {
    type T = EId

    override def fromInt(s: Int): EId               = EId.fromInt(s)
    override def toInt(lbl: T): Int                 = EId.toInt(lbl)
    override def toIntF[F[_]](fs: F[EId]): F[Int]   = EId.toIntF(fs)
    override def fromIntF[F[_]](fs: F[Int]): F[EId] = EId.fromIntF(fs)

    override def wrap(s: EId): T                 = s
    override def unwrap(lbl: T): EId             = lbl
    override def subst[F[_]](fs: F[EId]): F[T]   = fs
    override def unsubst[F[_]](fs: F[T]): F[EId] = fs
  }
  type VarId = VarId.T

  type Expr     = ExprF[EId]
  type Variable = InputF[EId]

  def root: EId
  def coalgebra: EId ==> Expr // equivalent to CoAlgebra[ExprF, EId]

  def ids: OpaqueIntSubset[EId]
  def variableIds: OpaqueIntSubset[VarId]

  def hylo[X](algebra: Algebra[ExprF, X]): EId ==> X = Ops.hylo(coalgebra, algebra)

  def memoizedHylo[X](algebra: Algebra[ExprF, X])(implicit classTag: ClassTag[X]): EId ==> X =
    Ops.memoizedHylo(coalgebra, algebra, ids.newCache[X])
}
