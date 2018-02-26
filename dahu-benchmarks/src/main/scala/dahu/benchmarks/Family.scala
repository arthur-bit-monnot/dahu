package dahu.benchmarks

import dahu.constraints.CSP
import dahu.model.compiler.Algebras
import dahu.model.input.Tentative
import dahu.model.interpreter.Interpreter
import dahu.model.types.Value
import dahu.utils.errors._

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

  def printSolutions[T](sat: Tentative[T], maxSolutions: Option[Int] = None): Unit = {
    val ast = Algebras.parse(sat)
    val csp = CSP.from(ast)
    val sols = mutable.ArrayBuffer[String]()
    val solutionString = (f: csp.Assignment) => {
      ast.variables.domain
        .toIterable()
        .map(v => (ast.variables(v), f.get(v)))
        .map {
          case (id, Some(value)) => s"${id.name}: $value"
          case (_, None)         => unexpected("Solution is partial")
        }
        .mkString("\t")
    }
    val evaluatedSolution: csp.Assignment => Either[Any, Value] = ass => {
      val f: ast.ID => Value = id =>
        csp
          .extractSolution(ass)
          .get(id)
          .getOrElse(unexpected("Some inputs are not encoded in the solution"))
      Interpreter.evalWithFailureCause(ast)(f)
    }
    csp.enumerateSolutions(
      onSolutionFound = f => {
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
      printSolutions(pb.pb, Some(1))
    }
  }

  def main(args: Array[String]): Unit = {
    solveAndPrintAll()
  }
}
