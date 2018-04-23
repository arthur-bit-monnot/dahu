package dahu.benchmarks

import dahu.model.compiler.Algebras
import dahu.model.input.Tentative
import dahu.model.interpreter.Interpreter
import dahu.model.types.Value
import dahu.solvers.{MetaSolver, PartialSolver}
import dahu.solvers.constraints.CSPPartialSolver
import dahu.z3.Z3PartialSolver

import scala.collection.mutable

abstract class Family(val familyName: String) {
  private var counter = 0
  val instancesMap: mutable.Map[String, SatProblem] = mutable.LinkedHashMap()

  def instance(instanceName: String)(f: => SatProblem): Unit = instances(instanceName)(Seq(f))

  def instances(instanceName: String)(f: => Seq[SatProblem]): Unit = {
    val pbs = f
    for(pb <- pbs) {
      val name = s"$counter-$instanceName"
      instancesMap += ((name, pb))
      counter += 1
    }
  }

  def defaultSolver: PartialSolver.Builder = Z3PartialSolver.builder

  def printSolutions[T](sat: Tentative[T], maxSolutions: Option[Int] = None): Unit = {
    val solver = MetaSolver.of(Algebras.parse(sat), defaultSolver)
    val sols = mutable.ArrayBuffer[String]()
    val solutionString = (f: solver.ast.Assignment) => {
      solver.ast.variables.domain
        .toIterable()
        .map(v => (solver.ast.variables(v), f(v)))
        .toList
        .sortBy(_._1.id)
        .map {
          case (id, value) => s"${id.id}: $value"
        }
        .mkString("  -  ")
    }
    val evaluatedSolution: solver.ast.Assignment => Interpreter.Result[Value] =
      (ass: solver.ast.Assignment) => {
        Interpreter.evalWithFailureCause(solver.ast)(ass)
      }
    solver.enumerateSolutions(
      onSolutionFound = (f: solver.ast.Assignment) => {
        sols += solutionString(f) + "\n" + evaluatedSolution(f) + "\n"
      },
      maxSolutions = maxSolutions
    )
    println(s"Solutions found: ${sols.size} ${maxSolutions.map("(max:" + _ + ")").getOrElse("")}")
    sols.sorted.foreach(println)
  }

  def solveAndPrintAll(): Unit = {
    println(familyName)
    for((name, pb) <- instancesMap) {
      println(s"instance: $name")
      printSolutions(pb.pb, Some(1))
    }
  }

  def main(args: Array[String]): Unit = {
    solveAndPrintAll()
  }
}
