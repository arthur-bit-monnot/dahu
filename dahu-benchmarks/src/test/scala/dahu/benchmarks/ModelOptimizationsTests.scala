package dahu.benchmarks

import cats.Id
import dahu.graphs.DAG
import dahu.model.ir._
import dahu.model.problem.SatisfactionProblem
import dahu.model.types.{TagIsoInt, Value}
import dahu.recursion.{FAlgebra, Fix}
import dahu.utils.errors._
import dahu.model.compiler.Optimizations.{simplifications, PASS, Tree}

import utest._

object ModelOptimizationsTests extends TestSuite {

  val rand = scala.util.Random

  def totalFormula(pb: SatProblem): Fix[Total] = {
    val parsed = dahu.model.compiler.Algebras.parse(pb.pb)
    SatisfactionProblem.encode(parsed.root, parsed.tree.asFunction, optimize = false).condition
  }

  def randomValue(t: TagIsoInt[_]): Value = {
    val size = t.max - t.min + 1
    val i = t.min + rand.nextInt(size)
    t.toValue(i)
  }

  val dag = new DAG[cats.Id, Fix[Total]] {

    /** Generation of a node from a node ID. */
    override def algebra: Fix[Total] => Id[Fix[Total]] = x => x

    /** All direct children of a node. */
    override def children(graph: Id[Fix[Total]]): Set[Fix[Total]] = {
      Fix.unfix(graph) match {
        case x: InputF[_]                 => Set[Fix[Total]]()
        case x: CstF[_]                   => Set[Fix[Total]]()
        case ProductF(members, _)         => members.toSet
        case ComputationF(_, args, _)     => args.toSet
        case OptionalF(value, present, _) => Set(value, present)
      }
    }
  }

  def randomInputs(tree: Fix[Total]): Map[String, Value] = {
    dag
      .descendantsAndSelf(tree)
      .map(Fix.unfix(_))
      .collect {
        case InputF(name, t: TagIsoInt[_]) => name -> randomValue(t)
        case InputF(name, t) =>
          unexpected(s"Cannot generate random values with a TagIsoInt. Got: $t")
      }
      .toMap
  }

  def eval(ast: Fix[Total], inputs: Map[String, Value]): Value = {
    val alg: FAlgebra[Total, Value] = {
      case x: InputF[_]             => inputs(x.name)
      case CstF(v, _)               => v
      case ComputationF(f, args, _) => Value(f.compute(args))
      case ProductF(members, t)     => Value(t.idProd.buildFromValues(members))
      case OptionalF(value, present, _) =>
        present match {
          case true  => Value(Some(value))
          case false => Value(None)
        }
    }
    dahu.recursion.Recursion.cata(alg)(ast)
  }

  val pbs = NumSolutionsTest.corpus.flatMap(_.instancesMap.values.toSeq)
  //  val optimizations = dahu.model.compiler.Optimizations.simplifications

  def optimize(ast: Fix[Total], passes: Seq[PASS]): Fix[Total] = {
    val alg = passes
      .fold((x: Tree) => x)(_ andThen _)
      .andThen(x => Fix(x))
    dahu.recursion.Recursion.cata[Total, Fix[Total]](alg)(ast)
  }

  val optimizers: Seq[Seq[PASS]] =
    simplifications.distinct.map(Seq(_)) :+ simplifications

  val numTests = 10

  def tests = Tests {

    "same-results-between optimized and unoptimized" - {
      for(optimizer <- optimizers) {
        // println(s"Optimizer: $optimizer")
        for(pb <- pbs) {
          val ast = totalFormula(pb)
          val optimizedAst = optimize(ast, optimizer)

          for(i <- 0 until numTests) {
            val input = randomInputs(ast)

            val r1 = eval(ast, input)
            val r2 = eval(optimizedAst, input)
            r1 ==> r2
          }
        }
      }
    }
  }
}
