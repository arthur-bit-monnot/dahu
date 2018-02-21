package dahu.benchmarks

import dahu.constraints.CSP
import dahu.model.input._
import dahu.model.compiler.Algebras._
import dahu.model.types._
import utest._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object NumSolutionsTest extends TestSuite {

  val corpus: Seq[Family] = Seq(
    GraphColoring
  )

  def numSolutions(expr: Expr[Boolean], maxSolutions: Option[Int] = None): Int = {
    val asd = parse(expr)
    val csp = CSP.from(asd)
    csp.enumerateSolutions(maxSolutions = maxSolutions)
  }
  def printSolutions(sat: Expr[Boolean],
                     vars: Seq[Input[_]],
                     maxSolutions: Option[Int] = None): Unit = {
    val ast = parse(sat)
    val csp = CSP.from(ast)
    val sols = mutable.ArrayBuffer[String]()
    csp.enumerateSolutions(
      onSolutionFound = f => {
        val res = vars
          .map(v => (v, ast.fromInput(v).flatMap(id => f.get(id))))
          .map { case (id, value) => s"${id.name}: ${value.getOrElse("ANY")}" }
          .mkString("\t")
        sols += res
      },
      maxSolutions = maxSolutions
    )
    sols.sorted.foreach(println)
  }
  def tests = Tests {
    "corpus" - {
      "num-solutions" - {
        val results = for(fam <- corpus; (instanceName, instance) <- fam.instancesMap) yield {
          val res = Try {
            instance match {
              case SatProblem(pb, Exactly(n)) =>
                assert(numSolutions(pb) == n)
              case SatProblem(pb, AtLeast(n)) =>
                assert(numSolutions(pb, maxSolutions = Some(n)) >= n)
              case _ =>
                dahu.utils.Errors.unexpected("No use for problems with unkown number of solution.")
            }
          }
          (fam.familyName, instanceName, res)
        }
        val failures =
          results.map(_._3).collect { case Failure(e) => e }

        if(failures.nonEmpty) {
          // print summary of successes/failures and throw the first error
          for((fam, ins, res) <- results) {
            res match {
              case Success(_) => println(s"Success: $fam/$ins")
              case Failure(_) => println(s"FAILURE: $fam/$ins")
            }
          }
          failures.foreach(throw _)
        } else {
          // everything went fine, return a string recap of the problems tackled
          val stringResults: Seq[String] = for((fam, ins, res) <- results) yield {
            res match {
              case Success(_) => s"Success: $fam/$ins"
              case Failure(_) => dahu.utils.Errors.unexpected
            }
          }
          stringResults.mkString("\n")
        }
      }
    }
  }
}
