package dahu.solver

import dahu.expr.Expr
import dahu.expr.types.TagIsoInt
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

trait XSolver {

  type Solution = Int => Int
  type State = Int => Set[Int]
  val problem: Problem

  protected def state: State

  // create an initial state
  def initState

  // mutates state
  def solve(state: State): Unit

  def branchAndPropagate
}

abstract class Solver {

  def extend(i: Id, e: ExprF[Id]): Either[Error, Unit]

}

abstract class MetaCSP() extends Solver {
  val subsolvers: mutable.ArrayBuffer[Solver]

  override def extend(i: Id, e: ExprF[Id]): Either[Error, Unit] = ???
//  {
//    var i = 0
//    while(i < subsolvers.length) {
//      if(subsolvers(i).extend(i, e).isRight)
//        i = Integer.MAX_VALUE // exit loop
//      else
//        i += 1
//    }
//
//    for(i <- subsolvers.indices) {
//
//    }
//    val (applied, res) = subsolvers.foldLeft((false, Seq[Solver]())){
//      case ((true, seq), solver) => (true, seq :+ solver)
//      case ((false, seq), solver) => (solver.extend(i, e).isRight, seq :+ solver)
////        solver.extend(i, e) match {
////          case Left(err) => (false, seq :+ solver)
////          case Right(_) => (true, seq :+ solver)
////        }
//    }
//    if(applied)
//      Right(new MetaCSP(subsolvers))
//    else
//      Left(Error(s"No subsolver supporting $e"))
//  }

}


class CSPSolver extends Solver {
  override def extend(i: Id, e: ExprF[Id]): Either[Error, Unit] = e match {
    case InputF(_, typ) if typ.isInstanceOf[TagIsoInt[_]] =>
      Right(())
    case ComputationF(f, args, typ) =>

      Right(())
  }
}