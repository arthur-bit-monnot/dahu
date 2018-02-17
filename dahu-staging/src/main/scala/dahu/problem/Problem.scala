package dahu.problem

import dahu.recursion.Types._
import dahu.solver.Domain
import dahu.utils.debug._

trait Problem[T, V] {
  def vars: Array[KI[T]]
  def asVar(v: Int): Option[KI[T]] = slow { vars.find(_ == v) }
  def dom: KI[T] => Domain[V]
}
