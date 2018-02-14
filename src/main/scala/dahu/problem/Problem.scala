package dahu.problem

import dahu.recursion.Types.ExprId
import dahu.solver.Domain


trait Problem[V] {
  def vars: Array[ExprId]
  def hasVar(v: ExprId): Boolean
  def dom: ExprId => Option[Domain[V]]

}
