package dahu.planner

import dahu.model.compiler.Algebras
import dahu.model.interpreter.Interpreter
import dahu.model.interpreter.Interpreter.Res
import dahu.model.types.Value
import dahu.solvers.MetaSolver
import dahu.z3.Z3PartialSolver
import dahu.utils.errors._
import dahu.utils.debug._

import cats.implicits._

object Planner {

  val backend = Z3PartialSolver.builder

  def solve(chronicle: Chronicle, deadline: Long)(implicit cfg: Config): Option[String] = {
    if(System.currentTimeMillis() > deadline)
      return None
    val sat = chronicle.toSatProblem
    info("  Encoding...")
    val solver = MetaSolver.of(Algebras.parse(sat), backend)
    val evaluatedSolution: solver.ast.Assignment => Interpreter.Result[Value] =
      (ass: solver.ast.Assignment) => {
        Interpreter.evalWithFailureCause(solver.ast)(ass)
      }
    solver.nextSolution(deadline) match {
      case Some(ass) =>
        evaluatedSolution(ass) match {
          case Res(operators) =>
            Some(
              operators
                .asInstanceOf[Seq[Operator[cats.Id]]]
                .filter(_.present)
                .sortBy(_.start)
                .map {
                  case Operator(name, args, s, e, _) => s"[$s, $e] $name(${args.mkString(",")})"
                }
                .mkString("\n")
            )
          case x => unexpected(x.toString)
        }
      case None => None
    }
  }

}
