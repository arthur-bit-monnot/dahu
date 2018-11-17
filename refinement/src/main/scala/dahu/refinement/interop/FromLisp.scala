package dahu.refinement.interop

import cats._
import cats.implicits._
import com.vividsolutions.jts.geom.Geometry
import dahu.graphs.{ASG, IDTop, OpenASG}
import dahu.graphs.transformations.Transformation
import dahu.lisp.compile.Env._
import dahu.lisp.compile._
import dahu.model.compiler.Algebras
import dahu.model.functions
import dahu.model.functions.{->:, Fun2, FunAny}
import dahu.model.input.Lambda.LambdaIdent
import dahu.model.input.{Ident, Scope, TypedIdent}
import dahu.model.ir._
import dahu.model.math._
import dahu.model.math.sequence.{FirstMatches, LastMatches}
import dahu.model.problem.{API, PartialEval}
import dahu.model.products.{Field, FieldAccessAny, ProductTagAny}
import dahu.model.types._
import dahu.recursion.FAlgebra
import dahu.refinement.common.{Params, R, Values}
import dahu.refinement.interop.evaluation.{CompileEnv, CompiledFun, CompiledFunBridge}
import dahu.refinement.la.LeastSquares
import dahu.refinement.{MemImpl, RMemory, RefExpr}
import dahu.utils._
import dahu.utils.errors._

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

object FromLisp extends App {

  val env = sources.default()
  implicit val ctx = new Context[Env.DefaultID](env, None)

//  evalFile("refinement/domains/prelude.clj")

  dahu.lisp.compile.evalMany(sources.prelude)
//  dahu.lisp.compile.evalMany(sources.testDomain)

  dahu.lisp.compile.evalFile("refinement/domains/car3.clj")

  val cstate = dahu.lisp.compile.parseEval("^cstate", ctx) match {
    case Success(CstF(tpe: ProductTagAny, _)) => tpe
    case Success(x) =>
      errors.unexpected(s"^cstate does not point to a valid record but to $x")
    case Failure(e) => throw e
  }
  println(cstate)

//  val mem = new MemImpl()

  val _withParser = ctx.optGraph.fixID.flatPrepro[String](str => Try(dahu.lisp.parse(str)))

  val _asg = _withParser //.transform(transformations.asErrors)
  val asg = _asg.fixID

  val x = new MemImpl()
  x.print()

  val simplified = OpenASG.ana(asg.internalCoalgebra)

  val problem = new ContinuousProblem[asg.ID](simplified, cstate)

  val YY =
    """(map
         ((meta.as-lambda current-events
                          (meta.as-lambda current-discrete-state constraints))
          NO_EVENTS)
         discrete-state-trajectory)""".stripMargin

  def load() = {
    (for {
      dstates <- asg.getTreeRoot("discrete-state-trajectory")
      constraints <- asg.getTreeRoot("band-constraints")
      pb <- problem.createProblem(dstates, constraints)
    } yield pb) match {
      case Success(pb) =>
        val solver = new Solver(pb, Params())
        solver.solve()
      case Failure(e) => throw e
    }
  }

  println(load())
  sys.exit(0)

}

import transformations._

case class Band(
    constraintsStart: Seq[Constraint],
    constraintsMiddle: Seq[Constraint],
    constraintsEnd: Seq[Constraint]
)

case class Problem(
    cstate: ProductTagAny,
    bands: Seq[Band]
)

class ContinuousProblem[X](_asg: ASG[X, ExprF, Id], cstate: ProductTagAny) {
  private val asg = _asg.fixID

  val openHeadTails = asg
    .transform(withHeadTail(cstate, 1))
//    .transform(scalarize)
    .transform(optimizer)
//    .transform(scalarize)
    .transform(optimizer)
    .manualMap(PartialEval)
    .manualMap(PartialEval)
    .transform(optimizer)
    .transform(optimizer)
    .transform(optimizer)
    .manualMap(PartialEval)
    .transform(optimizer)
    .manualMap(PartialEval)
    .transform(optimizer)
    .manualMap(PartialEval)
    .transform(optimizer)
    .transform(asErrors)
    .transform(optimizer)
    .transform(optimizer)
  val headTails = openHeadTails.fixID

  val printer = headTails.cata(Algebras.printAlgebraTree)
  def echo(x: X): Unit = println(printer.get(x).mkString(50))
  def echoI(x: I): Unit = println(printer.getInternal(x).mkString(50))

  type I = headTails.ID
  type Tagged[A] = A

  def createProblem(dstates: X, constraints: X): Try[Problem] =
    Try {
      echo(dstates)
      echo(constraints)

      val ds = headTails.getExt(dstates) match {
        case SequenceF(members, tpe) => members
        case _                       => unexpected
      }
//    val c0: ExprF[I] = headTails.getExt(constraints0)
//    val a = c0.smap(headTails.internalCoalgebra)
      def unnest2(i: I): ExprF[ExprF[I]] = {
        headTails
          .internalCoalgebra(i)
          .smap(i => headTails.internalCoalgebra(i))
      }
      def unnest3(i: I) = { //: ExprF[ExprF[I]] = {
        headTails
          .internalCoalgebra(i)
          .smap(i => headTails.internalCoalgebra(i).smap(headTails.internalCoalgebra))
      }
//    println(unnest(headTails.getTreeRoot(constraints0)))
//    sys.exit(0)
      val c0s = headTails.getExt(constraints) match {
        case SequenceF(members, _) => members
        case _                     => unexpected
      }
      require(ds.size == c0s.size)

      val compiler = new Compiler[I](headTails.internalCoalgebra, cstate)
      val bands: Seq[Band] = for(i <- ds.indices) yield {
        println(" ---- BAND ---- " + i)

        def asConstraints(i: I): Seq[Constraint] = {
          headTails.internalCoalgebra(i) match {
            case SequenceF(members, _) =>
              for(c <- members.toSeq) yield {
                println(unnest3(c))
                compiler.compileQualified(c)
              }

            case _ => unexpected
          }
        }

        val band = headTails.internalCoalgebra(c0s(i)) match {
          case SequenceF(members, _) =>
            members.map(asConstraints) match {
              case Vec(start, middle, end) => Band(start, middle, end)
            }

          case _ => unexpected
        }
        band
      }

      Problem(cstate, bands)
    }

}

case class Constraint(fun: CompiledFunBridge)

class Compiler[I](_coalg: I => ExprF[I], cstate: ProductTagAny) {
  val _tree = OpenASG.ana(_coalg)
  val tree = _tree.fixID

  val coalg = tree.internalCoalgebra _
  type ID = tree.ID

  def compileQualified(i: I): Constraint = {
    val compiled = compileLambda(tree.getTreeRoot(i))
    Constraint(compiled)

  }
  def compileLambda(i: ID): CompiledFunBridge = {
    evaluation.compile[ID](coalg, cstate)(i)
  }
}
