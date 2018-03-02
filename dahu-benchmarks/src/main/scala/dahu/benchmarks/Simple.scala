package dahu.benchmarks

import dahu.model.compiler.Algebras
import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.interpreter.Interpreter
import dahu.model.types.Value
import dahu.solvers.MetaSolver
import dahu.z3.Z3PartialSolver

import scala.collection.mutable

object Simple extends Family("simple") {
  var counter = 0
  def variable() =
    Input[Int]({ counter += 1; "v" + counter.toString }).subjectTo(x => x >= 0 && x <= 10)

  val v1 = variable()
  val v2 = variable().subjectTo(_ > v1)

  instances("base")(
    Seq(
      SatProblem(v1 < 100, NumSolutions.Exactly(11)),
      SatProblem(v1 === 1, NumSolutions.Exactly(1)),
      SatProblem(v1 <= 1, NumSolutions.Exactly(2)),
      SatProblem(v2 === 1, NumSolutions.Exactly(1)),
      SatProblem(v2 <= 1, NumSolutions.Exactly(1)),
      SatProblem(v2 <= 2, NumSolutions.Exactly(3)),
    ))

}

object Z3Simple extends App {

  val pb = Cst("Success").subjectTo(_ => Simple.v2 === 1)
  val builder = Z3PartialSolver.builder

  val solver = MetaSolver.of(Algebras.parse(pb), builder)
  val sols = mutable.ArrayBuffer[String]()
  val solutionString = (f: solver.ast.Assignment) => {
    solver.ast.variables.domain
      .toIterable()
      .map(v => (solver.ast.variables(v), f(v)))
      .map {
        case (id, value) => s"${id.name}: $value"
      }
      .mkString("\t")
  }
  val evaluatedSolution: solver.ast.Assignment => Either[Any, Value] =
    (ass: solver.ast.Assignment) => {
      Interpreter.evalWithFailureCause(solver.ast)(ass)
    }
  solver.enumerateSolutions(
    onSolutionFound = (f: solver.ast.Assignment) => {
      sols += solutionString(f) + "\n" + evaluatedSolution(f) + "\n"
    },
    maxSolutions = Some(1)
  )
  println(s"Solutions found: ${sols.size}")
  sols.sorted.foreach(println)
}
