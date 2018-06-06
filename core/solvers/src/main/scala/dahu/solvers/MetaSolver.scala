package dahu.solvers

import dahu.graphs.DAG
import dahu.model.input.{Expr, Ident, Input, TypedIdent}
import dahu.model.interpreter.Interpreter
import dahu.model.ir.Total
import dahu.model.problem.API
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types._
import dahu.utils.errors._

import scala.concurrent.duration.Deadline

class MetaSolver(val e: Expr[_], val builder: PartialSolver.Builder) {
  val pb = API.parseAndProcess(e).fixID
  lazy val valuesTree = pb.mapExternal[cats.Id](_.value)

  val solver = builder(pb)

  def defaultDomain(k: Expr[_]): Stream[Value] = k.typ match {
    case t: TagIsoInt[_] =>
      assert(t.min <= t.max, "empty default domain")
      (t.min to t.max).toStream.map(i => t.toValue(i)) //TODO
    case _ => ???
  }

  def nextSolution(deadline: Option[Deadline] = None): Option[Expr[_] => Value] =
    solver.nextSatisfyingAssignment(deadline) match {
      case Some(assignment) =>
        val total = (x: Expr[_]) => assignment(x).getOrElse(defaultDomain(x).head) // TODO: use head option or fail early if an input has an empty domain
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
      val evaluation = pb.tree.cata(Interpreter.evalAlgebra(assignment))
      evaluation.get(e) match {
        case IR(_, false, _)   => Interpreter.Empty
        case IR(_, _, false)   => Interpreter.ConstraintViolated
        case IR(v, true, true) => Interpreter.Res(v.asInstanceOf[A])
        case _                 => unexpected("A bool expression did not eval to false/true")
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
  def of(expr: Expr[_], builder: PartialSolver.Builder): MetaSolver =
    new MetaSolver(expr, builder)
}
