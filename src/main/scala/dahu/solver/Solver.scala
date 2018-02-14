package dahu.solver

import dahu.expr.Expr
import dahu.expr.types.TagIsoInt
import dahu.recursion.Types.ExprId
import dahu.recursion.{ComputationF, ExprF, InputF}
import dahu.utils.Errors.Error

import scala.collection.mutable

object Types {
  type Id = Int
}
import Types._

abstract class Problem {

  def accepts(e: ExprF[Id]): Boolean

  // frontier
  //  - inputs: variables with no predecessors
  //  - all variables are potential outputs,
  //    effective outputs are the ones that are also input of other problems
}

abstract class MetaProblem extends Problem {
  def subproblems: Seq[Problem]

  override def accepts(e: ExprF[Id]): Boolean =
    subproblems.exists(_.accepts(e))
}

class IntConstraintSatisfactionProblem extends Problem {

  def fullyTranslatableToInts(e: ExprF[Id]): Boolean = ???

  override def accepts(e: ExprF[Id]): Boolean =fullyTranslatableToInts(e)
}

trait Domain[V]

trait Solver[V, D <: Domain[V]] {

  def enforce(variable: ExprId, domain: D)
  def solve: Option[ExprId => V]
}
