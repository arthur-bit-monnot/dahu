package dahu.model.transformations

import dahu.model.ir.{ExprF, Total}
import dahu.model.math.bool

trait OptimizationContext[I <: Int, F[X] >: Total[X] <: ExprF[X]] {
  def retrieve(i: I): F[I]
  def record(fi: F[I]): I

  lazy val TRUE: I = record(bool.TrueF)
  lazy val FALSE: I = record(bool.FalseF)
}
