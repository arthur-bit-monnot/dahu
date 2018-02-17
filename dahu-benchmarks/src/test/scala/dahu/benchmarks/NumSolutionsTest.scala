package dahu.benchmarks

import dahu.constraints.CSP
import dahu.expr.{Cst, Expr, Input}
import dahu.expr.types.TagIsoInt
import org.scalatest.FreeSpec
import dahu.expr.dsl._
import dahu.solver
import dahu.recursion.Types._
import dahu.recursion.Algebras._

import scala.collection.mutable

class NumSolutionsTest extends FreeSpec {

  val corpus: Seq[Family] = Seq(
    GraphColoring
  )

  def numSolutions(expr: Expr[Boolean], maxSolutions: Option[Int] = None): Int = {
    val asd = transpile(expr, coalgebra)
    val csp = CSP.from(asd)
    csp.enumerateSolutions(maxSolutions = maxSolutions)
  }
  def printSolutions(sat: Expr[Boolean],
                     vars: Seq[Input[_]],
                     maxSolutions: Option[Int] = None): Unit = {
    val asd  = transpile(sat, coalgebra)
    val csp  = CSP.from(asd)
    val sols = mutable.ArrayBuffer[String]()
    csp.enumerateSolutions(
      onSolutionFound = f => {
        val res = vars
          .map(v => (v, asd.compiledForm(v).flatMap(id => f.get(ExprId.toInt(id)))))
          .map { case (id, value) => s"${id.name}: ${value.getOrElse("ANY")}" }
          .mkString("\t")
        sols += res
      },
      maxSolutions = maxSolutions
    )
    println(sols.sorted.mkString("\n"))
  }

  for(fam <- corpus) {
    fam.familyName - {
      for((instanceName, instance) <- fam.instancesMap) {
        instanceName in {
          instance match {
            case SatProblem(pb, Exactly(n)) =>
              assert(numSolutions(pb) == n)
            case SatProblem(pb, AtLeast(n)) =>
              assert(numSolutions(pb, maxSolutions = Some(n)) >= n)
            case _ =>
              dahu.utils.Errors.unexpected("No use for problems with unkown number of solution.")
          }
        }
      }
    }
  }
}
