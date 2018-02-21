package dahu.solvers

import dahu.constraints.CSP
import dahu.constraints.domains.{IntDomain, IntervalDomain}
import dahu.constraints.interval.Interval
import dahu.maps.{ArrayMap, SubInt, SubSubInt}
import dahu.model.ir.AST
import dahu.model.types._
import dahu.problem.IntCSP
import dahu.utils.errors._
import spire.math.Trilean

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

trait Solver[K <: SubInt, V, D] {
  type Assignment = ArrayMap.Aux[K, V]
  type Solution = ArrayMap.Aux[K, Value]

  def domainIso: DomainIso[D, V]

  def enforce(variable: K, domain: D)
  def nextSolution(): Option[Assignment]

  def extractSolution(assignment: Assignment): Solution

  def consistent: Trilean
}

class MetaSolver1[K <: SubInt](asg: AST.Aux[_, K]) extends Solver[K, Any, Domain[Any]] {
  override def domainIso: DomainIso[Domain[Any], Any] = DomainIso.identity

  trait FirstTag
  type T1 = SubSubInt[K, FirstTag]
  implicitly[T1 <:< K] // simple compile time check

  def unsafe(id: K): T1 = id.asInstanceOf[T1]
  def typeOf(id: K): dahu.model.types.Tag[_] = asg.tree(id).typ
  val intSubProblem
    : IntCSP[T1] = IntCSP.intSubProblem(asg).asInstanceOf[IntCSP[T1]] // TODO do safely
  val solver: CSP[T1] = intSubProblem.getSolver

  override def enforce(variable: K, domain: Domain[Any]): Unit = {
    val tmp = domain.asInstanceOf[IntDomain]
    solver.enforce(unsafe(variable), Interval(tmp.lb, tmp.ub))
  }

  override def nextSolution(): Option[ArrayMap.Aux[K, Any]] = solver.nextSolution() match {
    case Some(f) =>
      val f2 = f.mapFromKey {
        case (k, v) =>
          typeOf(k) match {
            case t: TagIsoInt[_] => t.fromInt(v)
            case _               => unexpected("Int value whose type is not isomorphic to Int")
          }
      }
      Some(f2.asInstanceOf[ArrayMap.Aux[K, Any]])

    case None => None
  }

  override def extractSolution(assignment: Assignment): Solution = ???

  override def consistent: Trilean = solver.consistent
}
