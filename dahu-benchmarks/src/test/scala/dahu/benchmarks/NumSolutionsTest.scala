package dahu.benchmarks

import dahu.model.input._
import dahu.model.compiler.Algebras._
import dahu.model.interpreter.Interpreter
import dahu.model.types._
import dahu.solvers.MetaSolver
import dahu.solvers.constraints.CSPPartialSolver
import utest._

import scala.util.{Failure, Success, Try}

object NumSolutionsTest extends TestSuite {

  val corpus: Seq[Family] = Seq(
    Simple,
    GraphColoring,
    Jobshop
  )

  def numSolutions[T](expr: Tentative[T], maxSolutions: Option[Int] = None): Int = {
    val solver = MetaSolver.of(parse(expr), CSPPartialSolver.builder)
    val solutionString = (f: solver.ast.Assignment) => {
      solver.ast.variables.domain
        .toIterable()
        .map(v => (solver.ast.variables(v), f(v)))
        .map {
          case (id, value) => s"${id.name}: $value"
        }
        .mkString("\n")
    }
    val validateSolution: solver.ast.Assignment => Unit = ass => {
      val f: solver.ast.VID => Value = ass
      Interpreter.evalWithFailureCause(solver.ast)(f) match {
        case Right(1) =>
        case x =>
          System.err.println("Error: the following solution evaluates as not valid.")
          System.err.println(solutionString(ass))
          dahu.utils.errors.unexpected(s"Invalid solution. Result: $x")
      }
    }
    solver.enumerateSolutions(maxSolutions = maxSolutions, validateSolution)
  }

  def tests = Tests {
    "corpus" - {
      "num-solutions" - {
        val results = for(fam <- corpus; (instanceName, instance) <- fam.instancesMap) yield {
          val res = Try {
            val pb = instance.pb
            instance match {
              case SatProblem(_, NumSolutions.Exactly(n)) =>
                val res = numSolutions(pb)
                assert(res == n)
              case SatProblem(_, NumSolutions.AtLeast(n)) =>
                val res = numSolutions(pb, maxSolutions = Some(n))
                assert(res >= n)
              case _ =>
                dahu.utils.errors.unexpected("No use for problems with unkown number of solution.")
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
              case Failure(_) => dahu.utils.errors.unexpected
            }
          }
          stringResults.mkString("\n")
        }
      }
    }
  }
}
