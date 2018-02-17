package dahu.solver

import dahu.constraints.CSP
import dahu.constraints.domains.{IntDomain, IntervalDomain}
import dahu.constraints.interval.Interval
import dahu.expr.types.{Tag, TagIsoInt}
import dahu.problem.{IntCSP, IntProblem}
import dahu.recursion.ASDAG
import dahu.recursion.Types._
import dahu.structures.ArrayIntFunc
import dahu.utils.Errors._
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
    override def to(d: Domain[Any]): Domain[Any]   = d
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

trait Solver[Tag, V, D] {

  def domainIso: DomainIso[D, V]

  def enforce(variable: KI[Tag], domain: D)
  def nextSolution(): Option[ArrayIntFunc[Tag, V]]

  def consistent: Trilean
}

class MetaSolver1[Tag](asg: ASDAG[_]) extends Solver[Tag, Any, Domain[Any]] {
  override def domainIso: DomainIso[Domain[Any], Any] = DomainIso.identity

  type T1 = Tag with Integer
  def trans(id: KI[T1]): KI[Tag] =
    id.asInstanceOf[KI[Tag]] // ideally KI[T1] should be a subtype of KI[Tag]
  def unsafe(id: KI[Tag]): KI[T1]               = id.asInstanceOf[KI[T1]]
  def typeOf(id: KI[_]): dahu.expr.types.Tag[_] = asg.coalgebra(id.asInstanceOf[ExprId]).typ
  val intSubProblem: IntCSP[T1]                 = IntCSP.intSubProblem(asg)(_ => true)
  val solver: CSP[T1]                           = intSubProblem.getSolver

  override def enforce(variable: KI[Tag], domain: Domain[Any]): Unit = {
    val tmp = domain.asInstanceOf[IntDomain]
    solver.enforce(unsafe(variable), Interval(tmp.lb, tmp.ub))
  }

  override def nextSolution(): Option[ArrayIntFunc[Tag, Any]] = solver.nextSolution match {
    case Some(f) =>
      val f2 = f.mapFromKey {
        case (k, v) =>
          typeOf(k) match {
            case t: TagIsoInt[_] => t.fromInt(v)
            case _               => unexpected("Int value whose type is not isomorphic to Int")
          }
      }
      Some(f2.asInstanceOf[ArrayIntFunc[Tag, Any]])

    case None => None
  }

  override def consistent: Trilean = solver.consistent
}
