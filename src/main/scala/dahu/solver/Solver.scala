package dahu.solver

import dahu.constraints.CSP
import dahu.constraints.domains.IntDomain
import dahu.expr.types.TagIsoInt
import dahu.problem.{IntCSP, IntProblem}
import dahu.recursion.ASDAG
import dahu.recursion.Types._
import dahu.structures.ArrayIntFunc
import dahu.utils.Errors._
import spire.math.Trilean


trait Domain[V]

trait Solver[Tag, V, D <: Domain[V]] {

  def enforce(variable: KI[Tag], domain: D)
  def solve: Option[ArrayIntFunc[Tag, V]]

  def consistent: Trilean
}


class MetaSolver1[Tag](asg: ASDAG[_]) extends Solver[Tag, Any, Domain[Any]] {

  type T1 = Tag with Integer
  def trans(id: KI[T1]): KI[Tag] = id.asInstanceOf[KI[Tag]] // ideally KI[T1] should be a subtype of KI[Tag]
  def unsafe(id: KI[Tag]): KI[T1] = id.asInstanceOf[KI[T1]]
  def typeOf(id: KI[_]): dahu.expr.types.Tag[_] = asg.coalgebra(id.asInstanceOf[ExprId]).typ
  val intSubProblem: IntCSP[T1] = IntCSP.intSubProblem(asg)(_ => true)
  val solver: CSP[T1] = intSubProblem.getSolver


  override def enforce(variable: KI[Tag], domain: Domain[Any]): Unit = solver.enforce(unsafe(variable), domain.asInstanceOf[IntDomain])

  override def solve: Option[ArrayIntFunc[Tag, Any]] = solver.solve match {
    case Some(f) =>
      val f2 = f.mapFromKey {
        case (k, v) => typeOf(k) match {
          case t: TagIsoInt[_] => t.fromInt(v)
          case _ => unexpected("Int value whose type is not isomorphic to Int")
        }
      }
      Some(f2.asInstanceOf[ArrayIntFunc[Tag, Any]])

    case None => None
  }

  override def consistent: Trilean = solver.consistent
}