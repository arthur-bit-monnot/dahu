package dahu.benchmarks

import dahu.constraints.CSP
import dahu.model.compiler.Algebras
import dahu.model.input.Tentative

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

  def printSolutions(sat: Tentative[Boolean], maxSolutions: Option[Int] = None): Unit = {
    val ast = Algebras.parse(sat)
    val csp = CSP.from(ast)
    val sols = mutable.ArrayBuffer[String]()
    csp.enumerateSolutions(
      onSolutionFound = f => {
        val res = ast.variables.domain
          .toIterable()
          .map(v => (ast.variables(v), csp.extractSolution(f).get(v)))
          .map { case (id, value) => s"${id.name}: $value" }
          .mkString("\t")
        sols += res
      },
      maxSolutions = maxSolutions
    )
    println(s"Solutions found: ${sols.size} ${maxSolutions.map("(max:" + _ + ")").getOrElse("")}")
    sols.sorted.foreach(println)
  }

  def solveAndPrintAll(): Unit = {
    println(familyName)
    for((name, pb) <- instancesMap) {
      printSolutions(pb.formula, Some(1))
    }
  }

  def main(args: Array[String]): Unit = {
    solveAndPrintAll()
  }
}
