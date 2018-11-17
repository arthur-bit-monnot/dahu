package dahu.solvers

import cats.Id
import dahu.graphs.DAG
import dahu.model.input._
import dahu.model.interpreter.{FEval, Interpreter, PEval}
import dahu.graphs._
import dahu.model.compiler.Algebras
import dahu.model.ir.{CstF, Total}
import dahu.model.math.bool
import dahu.model.types._
import dahu.solvers.problem.EncodedProblem
import dahu.solvers.solution.{Sol, Solution, SolverResult, TimeoutOrUnsat}
import dahu.utils._

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

  def solutionTrees(clause: Option[Expr[Bool]] = None,
                    deadline: Option[Deadline] = None): SolverResult =
    solver.nextSatisfyingAssignmentInternal(clause, deadline) match {
      case Some(assignment) =>
        val sol = pb.partialBind(i => {
          assignment(i).map(v => {
            CstF(v, pb.internalCoalgebra(i).typ)
          })
        })

        val nextSolution: () => SolverResult = () => {
          import dahu.model.input.dsl._
          val evaluation = sol.cata(Interpreter.evalAlgebra)
          println(e.decisionVariables)
          val disjuncts = for((p, v) <- e.decisionVariables) yield {

            val currentValue = evaluation.get(v) match {
              case FEval(x) => Cst(x, v.typ)
              case _        => dahu.utils.errors.unexpected(s"Decision variable not bound: $v")
            }
            val disjunct = p ==> !(v ==== currentValue)
//            println(s"$p $v")
//            println(evaluation.get(p))
//            println(evaluation.get(v))
//            println(disjunct)
            disjunct
          }
          val clause = bool.Or(disjuncts: _*)
          solutionTrees(Some(clause), deadline)
        }
        Sol(sol, nextSolution)
      case None => TimeoutOrUnsat
    }

  def nextSolution(deadline: Option[Deadline] = None): Option[Solution] =
    solver.nextSatisfyingAssignmentInternal(None, deadline) match {
      case Some(assignment) =>
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
