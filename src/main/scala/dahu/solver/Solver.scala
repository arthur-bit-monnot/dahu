package dahu.solver

import dahu.constraints.IntFunc
import dahu.recursion.Types._
import spire.math.Trilean


trait Domain[V]

trait Solver[Tag, V, D <: Domain[V]] {

  def enforce(variable: KI[Tag], domain: D)
  def solve: Option[IntFunc.Aux[Tag, V]]

  def consistent: Trilean
}
