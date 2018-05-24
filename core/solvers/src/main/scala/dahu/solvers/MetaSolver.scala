package dahu.solvers

import dahu.graphs.DAG
import dahu.model.input.{Expr, Input}
import dahu.model.interpreter.Interpreter
import dahu.model.ir.Total
import dahu.model.problem.API
import dahu.model.types.{TagIsoInt, Value}

import scala.concurrent.duration.Deadline

class MetaSolver(val e: Expr[_], val builder: PartialSolver.Builder) {
  val pb = API.parseAndProcess(e).fixID

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

  def solutionEvaluator[A](e: Expr[A]): (Expr[_] => Value) => Interpreter.Result[A] =
    ass => {
      implicit val tn = Total.treeNodeInstance
      val assignment = DAG[cats.Id, Expr[Any]]
        .descendantsAndSelf(e)
        .collect { case i: Input[Any] => i }
        .map(i => (i.id, ass(i)))
        .toMap
      println(assignment)
      API.eval(e, assignment(_))
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
