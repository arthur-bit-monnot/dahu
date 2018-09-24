package dahu.solvers

import dahu.graphs.DAG
import dahu.model.input.{Expr, Ident, Input, TypedIdent}
import dahu.model.interpreter.{FEval, Interpreter}
import dahu.model.ir.Total
import dahu.model.problem.API
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types._
import dahu.utils.errors._

import scala.concurrent.duration.Deadline

class MetaSolver(val e: Expr[Any],
                 val rootSolverFactory: PartialSolver.Builder,
                 val secondarySolverFactories: List[PartialSolver.Builder]) {
  val _pb = API.parseAndProcess(e) // provides a stable identifier
  val pb = _pb.fixID
  lazy val valuesTree = pb.mapExternal[cats.Id](_.value)

  val solver: PartialSolver[Expr[Any], pb.ID] = rootSolverFactory(pb)
  val subSolvers: List[PartialSolver[Expr[Any], pb.ID]] =
    secondarySolverFactories.map(factory => factory(pb))

  private def toValueKey(e: Expr[_]): pb.tree.ID = pb.tree.getTreeRoot(e).value
  private def toValidKey(e: Expr[_]): pb.tree.ID = pb.tree.getTreeRoot(e).valid

  def defaultDomain(k: Expr[_]): Stream[Value] = k.typ match {
    case t: TagIsoInt[_] =>
      assert(t.min <= t.max, "empty default domain")
      (t.min to t.max).toStream.map(i => t.toValue(i)) //TODO
    case _ => ???
  }

  def nextSolution(deadline: Option[Deadline] = None): Option[Expr[_] => Value] =
    solver.nextSatisfyingAssignmentInternal(deadline) match {
      case Some(assignment) =>
//        for(i <- pb.tree.internalBottomUpTopologicalOrder(toValidKey(pb.root))) {
//          println(
//            "XX: " + i + s" (${solver.internalRepresentation(i)}) : " + assignment(i) + " ---- " + pb.tree
//              .internalCoalgebra(i))
//        }
        val total = (x: Expr[_]) => assignment(toValueKey(x)).getOrElse(defaultDomain(x).head) // TODO: use head option or fail early if an input has an empty domain
        Some(total)
      case None => None
    }

  type Assignment = Expr[_] => Value

  def solutionEvaluator[A](ass: Expr[_] => Value): Expr[A] => Interpreter.Result[A] =
    e => {
      implicit val tn = Total.treeNodeInstance
      val assignment: Map[TypedIdent[Any], Value] = DAG[cats.Id, Expr[Any]]
        .descendantsAndSelf(e)
        .collect { case i: Input[Any] => i }
        .map(i => (i.id, ass(i)))
        .toMap
      val evaluation = pb.tree.cata(Interpreter.evalAlgebra(assignment.get))
      evaluation.get(e) match {
        case IR(_, FEval(false), _)                 => Interpreter.Empty
        case IR(_, _, FEval(false))                 => Interpreter.ConstraintViolated
        case IR(FEval(v), FEval(true), FEval(true)) => Interpreter.Res(v.asInstanceOf[A])
        case _                                      => ???
      }
    }

  def enumerateSolutions(maxSolutions: Option[Int] = None,
                         onSolutionFound: Assignment => Unit = _ => ()): Int = {
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
  def of(expr: Expr[_],
         builder: PartialSolver.Builder,
         secondaryBuilders: List[PartialSolver.Builder] = Nil): MetaSolver =
    new MetaSolver(expr, builder, secondaryBuilders)
}
