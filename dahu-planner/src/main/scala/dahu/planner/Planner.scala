package dahu.planner

import dahu.model.compiler.Algebras
import dahu.model.interpreter.Interpreter
import dahu.model.types.Value
import dahu.solvers.MetaSolver
import dahu.z3.Z3PartialSolver

object Planner {

  val backend = Z3PartialSolver.builder

  def solve(chronicle: Chronicle): Option[Any] = {
    val sat = chronicle.toSatProblem
    val solver = MetaSolver.of(Algebras.parse(sat), backend)
    val evaluatedSolution: solver.ast.Assignment => Interpreter.Result[Value] =
      (ass: solver.ast.Assignment) => {
        Interpreter.evalWithFailureCause(solver.ast)(ass)
      }
    solver.nextSolution().map(evaluatedSolution)
  }

}
