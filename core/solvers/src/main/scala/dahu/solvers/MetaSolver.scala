package dahu.solvers

import dahu.graphs.DAG
import dahu.model.input.{Expr, Ident, Input, TypedIdent}
import dahu.model.interpreter.{FEval, Interpreter, PEval}
import dahu.graphs._
import dahu.model.types._
import dahu.solvers.problem.EncodedProblem
import dahu.solvers.solution.Solution
import dahu.utils.errors._

import scala.concurrent.duration.Deadline

class MetaSolver(val e: EncodedProblem[Any],
                 val rootSolverFactory: PartialSolver.Builder,
                 val secondarySolverFactories: List[PartialSolver.Builder]) {
  lazy val pb = e.asg.fixID

  val solver: PartialSolver[Expr[Any], pb.ID] = rootSolverFactory(pb.rootedAt(e.sat))
  val subSolvers: List[PartialSolver[Expr[Any], pb.ID]] =
    secondarySolverFactories.map(factory => factory(pb.rootedAt(e.sat)))

  private def toKey(e: Expr[_]): pb.ID = pb.getTreeRoot(e)

  def defaultDomain(k: Expr[_]): Stream[Value] = k.typ match {
    case t: TagIsoInt[_] =>
      assert(t.min <= t.max, "empty default domain")
      (t.min to t.max).toStream.map(i => t.toValue(i)) //TODO
    case _ => ???
  }

  def nextSolution(deadline: Option[Deadline] = None): Option[Solution] =
    solver.nextSatisfyingAssignmentInternal(deadline) match {
      case Some(assignment) =>
//        for(i <- pb.internalBottomUpTopologicalOrder(toKey(e.sat))) {
//          println(
//            "XX: " + i + s" (${solver.internalRepresentation(i)}) : " + assignment(i) + " ---- " + pb
//              .internalCoalgebra(i))
//        }
//        for(i <- pb.internalBottomUpTopologicalOrder(toKey(e.res))) {
//          println(
//            "XX: " + i + s" (${solver.internalRepresentation(i)}) : " + assignment(i) + " ---- " + pb
//              .internalCoalgebra(i))
//        }
        def ass(id: pb.ID): Option[PEval[Any]] = assignment(id) match {
          case Some(v) => Some(FEval(v))
          case None    => None
        }

        val lazyMap = pb.cataWithPreFill(Interpreter.evalAlgebra, ass)
        Some(
          new Solution {
            override def eval[T](expr: Expr[T]): PEval[T] = lazyMap.get(expr).castUnsafe[T]
          }
        )
      case None => None
    }
  def enumerateSolutions(maxSolutions: Option[Int] = None,
                         onSolutionFound: Solution => Unit = _ => ()): Int = {
    var count = 0
    while(maxSolutions.forall(count < _)) {
      nextSolution() match {
        case Some(sol) =>
          onSolutionFound(sol)
          count += 1
        case None =>
          return count
      }
    }
    count
  }
}

object MetaSolver {
  def of(expr: EncodedProblem[Any],
         builder: PartialSolver.Builder,
         secondaryBuilders: List[PartialSolver.Builder] = Nil): MetaSolver =
    new MetaSolver(expr, builder, secondaryBuilders)
}
