package dahu.ast

import dahu.arrows.{==>, TypeInstances}
import dahu.recursion.{ExprF, InputF}

sealed abstract class IndexLabelImpl {
  type T
  def apply(s: Int): T
  def unwrap(lbl: T): Int
}

trait ASTable {

  /** Opaque type representing the IDs of the expression in this table. */
  val EId: IndexLabelImpl = new IndexLabelImpl {
    type T = Int
    override def apply(s: Int): T    = s
    override def unwrap(lbl: T): Int = lbl
  }
  type EId = this.EId.T

  type Expr     = ExprF[EId]
  type Variable = InputF[EId]

  def root: EId
  def arrow: EId ==> Expr

  def ids: TypeInstances[EId]
}
