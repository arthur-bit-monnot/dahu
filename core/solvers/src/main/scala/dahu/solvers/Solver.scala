package dahu.solvers

import dahu.constraints.domains.IntervalDomain
import dahu.constraints.interval.Interval
import dahu.utils.errors._

trait Domain[V] {
  def iterable: Iterable[V] = ???
}

trait DomainIso[D, V] {

  def to(d: Domain[V]): D
  def from(d: D): Domain[V]
}
object DomainIso {
  private val identityIso = new DomainIso[Domain[Any], Any] {
    override def to(d: Domain[Any]): Domain[Any] = d
    override def from(d: Domain[Any]): Domain[Any] = d
  }
  def identity[V]: DomainIso[Domain[V], V] = identityIso.asInstanceOf[DomainIso[Domain[V], V]]

  val intervalIso: DomainIso[Interval, Int] = new DomainIso[Interval, Int] {
    override def to(d: Domain[Int]): Interval = d match {
      case x: IntervalDomain => Interval(x.lb, x.ub)
      case _                 => unexpected("No completly faithfull translation of non interval domains")
    }
    override def from(d: Interval): IntervalDomain = IntervalDomain(d.lb, d.ub)
  }
}
