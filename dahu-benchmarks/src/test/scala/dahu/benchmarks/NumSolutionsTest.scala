package dahu.benchmarks

import dahu.model.input._
import dahu.model.compiler.Algebras._
import dahu.model.interpreter.Interpreter
import dahu.model.interpreter.Interpreter.Res
import dahu.model.types._
import dahu.solvers.{MetaSolver, PartialSolver}
import dahu.solvers.constraints.CSPPartialSolver
import dahu.z3.Z3PartialSolver
import utest._

object NumSolutionsTest extends TestSuite {

  val corpus: Seq[Family] = Seq(
//    Simple,
    Optionals,
//    GraphColoring,
//    Jobshop
  )

  val solvers = Seq(
    CSPPartialSolver.builder,
    Z3PartialSolver.builder
  )

  def numSolutions[T](expr: Tentative[T],
                      builder: PartialSolver.Builder,
                      maxSolutions: Option[Int] = None): Int = {
    val solver = MetaSolver.of(parse(expr), builder)
    val solutionString = (f: solver.ast.Assignment) => {
      solver.ast.variables.domain
        .toIterable()
        .map(v => (solver.ast.variables(v), f(v)))
        .map {
          case (id, value) => s"${id.id}: $value"
        }
        .mkString("\n")
    }
    val validateSolution: solver.ast.Assignment => Unit = ass => {
      val f: solver.ast.VID => Value = ass
      Interpreter.evalWithFailureCause(solver.ast)(f) match {
        case Res(_) =>
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
      val instances =
        for {
          fam <- corpus
          (instanceName, instance) <- fam.instancesMap.toIterable // conversion to iterable to preserve order when traversing LinkedHashMap
        } yield (s"${fam.familyName}/$instanceName", instance)

      "num-solutions-csp" - {
        val cspSolver = Z3PartialSolver.builder
        def test(pb: SatProblem): Unit = {
          pb match {
            case SatProblem(_, NumSolutions.Exactly(n)) =>
              val res = numSolutions(pb.pb, cspSolver, maxSolutions = Some(n + 1))
              assert(res == n)
            case SatProblem(_, NumSolutions.AtLeast(n)) =>
              val res = numSolutions(pb.pb, cspSolver, maxSolutions = Some(n))
              assert(res >= n)
            case _ =>
              dahu.utils.errors.unexpected("No use for problems with unknown number of solutions.")
          }
        }
        dahu.utils.tests.subtests[(String, SatProblem)](instances, x => test(x._2), _._1)
      }

      "solver-comparison" - {
        def test(pb: SatProblem): Unit = {
          numSolutions(pb.pb, Z3PartialSolver.builder, Some(1)) ==>
            numSolutions(pb.pb, CSPPartialSolver.builder, Some(1))
        }
        dahu.utils.tests.subtests[(String, SatProblem)](instances, x => test(x._2), _._1)
      }
    }
  }
}
