package dahu.planner

import dahu.model.compiler.Algebras
import dahu.model.interpreter.Interpreter
import dahu.model.interpreter.Interpreter.Res
import dahu.model.types.Value
import dahu.solvers.MetaSolver
import dahu.z3.Z3PartialSolver
import dahu.utils.errors._

object Planner {

  val backend = Z3PartialSolver.builder

  def solve(chronicle: Chronicle)(implicit cfg: Config): Option[String] = {
    val sat = chronicle.toSatProblem
    println("Building meta-solver...")
    val solver = MetaSolver.of(Algebras.parse(sat), backend)
    val evaluatedSolution: solver.ast.Assignment => Interpreter.Result[Value] =
      (ass: solver.ast.Assignment) => {
        Interpreter.evalWithFailureCause(solver.ast)(ass)
      }
    solver.nextSolution() match {
      case Some(ass) =>
        evaluatedSolution(ass) match {
          case Res(operators) =>
            Some(
              operators
                .asInstanceOf[List[Operator[cats.Id]]]
                .filter(_.present)
                .sortBy(_.start)
                .map {
                  case Operator(name, args, s, e, _) => s"[$s, $e] $name(${args.mkString(",")})"
                }
                .mkString("\n")
            )
          case _ => unexpected
        }
      case None => None
    }
  }

}
