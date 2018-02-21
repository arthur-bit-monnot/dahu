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
    GraphColoring,
    Jobshop
  )

  def numSolutions(expr: Expr[Boolean], maxSolutions: Option[Int] = None): Int = {
    val asd = parse(expr)
    val csp = CSP.from(asd)
    csp.enumerateSolutions(maxSolutions = maxSolutions)
  }

  def tests = Tests {
    "corpus" - {
      "num-solutions" - {
        val results = for(fam <- corpus; (instanceName, instance) <- fam.instancesMap) yield {
          val res = Try {
            instance match {
              case SatProblem(pb, NumSolutions.Exactly(n)) =>
                assert(numSolutions(pb) == n)
              case SatProblem(pb, NumSolutions.AtLeast(n)) =>
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
              case Success(_) => println(s"+ $fam/$ins")
              case Failure(_) => println(s"- $fam/$ins")
            }
          }
          failures.foreach(throw _)
        } else {
          // everything went fine, return a string recap of the problems tackled
          val stringResults: Seq[String] = for((fam, ins, res) <- results) yield {
            res match {
              case Success(_) => s"+ $fam/$ins"
              case Failure(_) => dahu.utils.Errors.unexpected
            }
          }
          stringResults.mkString("\n")
        }
      }
    }
  }
}
