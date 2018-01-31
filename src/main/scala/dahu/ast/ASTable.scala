package dahu.ast

import dahu.arrows.recursion.Algebra
import dahu.arrows.{==>, recursion, OpaqueIntSubset}
import dahu.recursion.{ExprF, InputF}

import scala.reflect.ClassTag

abstract class IndexLabelImpl {
  type T
  def apply(s: Int): T = fromInt(s)

  def fromInt(s: Int): T
  def toInt(lbl: T): Int
  def fromIntF[F[_]](fs: F[Int]): F[T]
  def toIntF[F[_]](fs: F[T]): F[Int]
}

abstract class SubIndexLabelImpl[UB] extends IndexLabelImpl {
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
  type EId = EId.T

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
  def coalgebra: EId ==> Expr // equivalent to Coalgebra[ExprF, EId]

  def variableCoalgebra: VarId ==> Variable = id => coalgebra(VarId.unwrap(id)).asInstanceOf[Variable]

  def ids: OpaqueIntSubset[EId]
  def variableIds: OpaqueIntSubset[VarId]

  def hylo[X](algebra: Algebra[ExprF, X]): EId ==> X = recursion.hylo(coalgebra, algebra)

  def memoizedHylo[X](algebra: Algebra[ExprF, X])(implicit classTag: ClassTag[X]): EId ==> X =
    recursion.memoizedHylo(coalgebra, algebra, ids.newCache[X])
}
