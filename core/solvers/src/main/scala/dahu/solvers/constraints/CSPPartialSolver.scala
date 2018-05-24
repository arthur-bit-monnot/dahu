package dahu.solvers.constraints

import dahu.constraints.interval.Interval
import dahu.model.ir.{Total, TotalSubAST}
import dahu.model.problem.IntBoolSatisfactionProblem
import dahu.model.problem.LazyTree
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types.{TagIsoInt, Value}
import dahu.solvers.PartialSolver
import dahu.solvers.problem.IntCSP
import dahu.utils.errors._

import scala.concurrent.duration.Deadline

class CSPPartialSolver[X](_ast: LazyTree[X, Total, IR, _]) extends PartialSolver[X] {

//  val intBoolPb = new IntBoolSatisfactionProblem[X](ast)
//
//  private val intPB = ??? //IntCSP.intProblem(intBoolPb)
//  private val csp = ??? // intPB.getSolver

  override def nextSatisfyingAssignment(
      deadlineMs: Option[Deadline]): Option[X => Option[Value]] = {
    ???
//    if(deadlineMs != -1)
//      dahu.utils.debug.warning("CSP Partial solver does not support deadlines yet.")
//    csp.nextSolution() match {
//      case Some(ass) =>
//        val partial: ast.PartialAssignment = (k: ast.VID) => {
//          ass
//            .get(k)
//            .map(v =>
//              ast.tree(k).typ match {
//                case t: TagIsoInt[_] => t.toValue(v)
//                case _               => unexpected
//            })
//        }
//        Some(partial)
//      case None => None
//    }
  }
}

object CSPPartialSolver {

  object builder extends PartialSolver.Builder {
    override def apply[X](ast: LazyTree[X, Total, IR, _]): CSPPartialSolver[X] =
      new CSPPartialSolver(ast)
  }
}
