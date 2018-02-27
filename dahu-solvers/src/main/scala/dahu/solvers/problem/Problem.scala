package dahu.solvers.problem

import dahu.maps.SubInt
import dahu.solvers.Domain
import dahu.utils.debug._

trait Problem[K <: SubInt, V] {
  def vars: Array[K]
  def asVar(v: Int): Option[K] = slow { vars.find(_ == v) }
  def dom: K => Domain[V]
}
