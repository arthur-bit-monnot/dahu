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
  implicit val ctx = new Context(env, Some(transformations.scalarize))

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

  def refexpr(str: String, mem: RMemory): Try[RefExpr] = {
    val tmp = RefExprs.asRefExpr(mem, str, asg)
//    println(tmp)
    tmp
  }

  val x = new MemImpl()
  x.print()

//  implicit class RefExprHelper(val sc: StringContext) extends AnyVal {
//    def str: String = sc.parts.mkString("")
//    def e(args: Any*): Try[RefExpr] = refexpr(sc.parts.mkString(""))
//
//    def eval2(args: Any*): Try[Value] =
//      asg.getTreeRoot(str).map { i =>
//        evaluation.eval(asg.internalCoalgebra, (id: TypedIdent) => Value(x.read(x.addressOf(id))))(
//          evaluation.EvalEnv.empty())(i)
//      }
//  }

  val simplified = OpenASG.ana(asg.internalCoalgebra)

  val problem = new ContinuousProblem[asg.ID](simplified, cstate)

  def enforce(source: String): Unit = {
    asg.getTreeRoot(source) match {
      case scala.util.Success(value) =>
        problem.addConstraints(value, tag = source)
      case scala.util.Failure(e) => throw e
    }
  }

  val YY =
    "(map (meta.as-lambda current-discrete-state constraints ) discrete-state-trajectory)"

  def load() = {
    (for {
      dstates <- asg.getTreeRoot("discrete-state-trajectory")
      constraints <- asg.getTreeRoot(YY)
      pb <- problem.createProblem(dstates, constraints)
    } yield pb) match {
      case Success(pb) =>
        val solver = new Solver(pb)
        solver.solve()
      case Failure(e) => throw e
    }
  }

//  enforce("constraints")
  println(load())
  sys.exit(0)
//  val res = problem.solve()
//  res.print()

//  enforce("(geom.is-in c1 pt1)")
//  enforce("(geom.is-in c1 pt2)")
//  enforce("(geom.is-in c2 pt3)")
//  enforce("(geom.is-not-in r1 pt1)")
//  enforce("(geom.is-in r2 pt3)")
//  enforce("(= (:x (first path)) 0.0)")
//  enforce("(= (:x (last path)) 10.0)")

//  implicit val memory = x
//
//  val ls = new LeastSquares(es.get)
//  ls.lmIteration(x, 10)
//
//  def print(str: String)(implicit mem: RMemory): Unit =
//    asg.getTreeRoot(str).map { i =>
//      eval(asg.internalCoalgebra, (id: TypedIdent) => Value(mem.read(mem.addressOf(id))))(
//        new EvalEnv())(i)
//    } match {
//      case scala.util.Success(value) => println(s"$str -> $value")
//      case scala.util.Failure(e)     => e.printStackTrace()
//    }
//
//  print("pt1")
//  print("pt2")
//  print("pt3")
//
//  print("(list pt1 pt2 pt3)")
//
//  print("(defvar ^points ps)")
//  print("(first-matches ps (fn [pt] (= (:x pt) 0.0)))")
//
//  println("-----")
//  x.print()

}

object RefExprs {
  def asRefExpr[X, I <: IDTop, Opt[_]: Functor](mem: RMemory,
                                                x: X,
                                                asg: OpenASG[X, ExprF, Opt, I]): Opt[RefExpr] = {
    asg
      .getTreeRoot(x)
      .map({ i =>
        val desc = asg.descendantsWithID(i).collect {
          case (_, InputF(x, _)) =>
            //            println(mem.addressOf(x))
            x
        }
        //        println(desc)
        val variableToArrayIndex = desc.zipWithIndex.toMap

        def valueOf(id: TypedIdent)(mem: Values): Value = {
          //          println(s"addr: ${variableToArrayIndex(id)}")
          val i = variableToArrayIndex(id)
          Value(mem(i))
        }

        def evaluator(mem: Values): Value = {
          evaluation.eval(asg.internalCoalgebra, valueOf(_)(mem))(evaluation.EvalEnv.empty())(i)
        }

        val f = new dahu.refinement.Fun {
          override def numParams: Int = variableToArrayIndex.size
          override def eval(params: Values): R =
            evaluator(params).asInstanceOf[R]
        }
        f.bind(desc.map(mem.addressOf): _*)
      })

  }
}

import transformations._

case class Band(
    constraints0: Seq[Constraint],
    constraints1: Seq[Constraint] = Seq(), //TODO: remove
    constraints2: Seq[Constraint] = Seq()
)

case class Problem(
    cstate: ProductTagAny,
    bands: Seq[Band]
)

class ContinuousProblem[X](_asg: ASG[X, ExprF, Id], cstate: ProductTagAny) {
  private val asg = _asg.fixID

  val openHeadTails = asg
    .transform(withHeadTail(cstate, 1))
    .transform(scalarize)
    .transform(optimizer)
    .transform(scalarize)
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
        println(i)
//      println(unnest3(c0s(i)))

        val c0si: Vec[I] = headTails.internalCoalgebra(c0s(i)) match {
          case SequenceF(members, _) => members
          case _                     => unexpected
        }
        c0si.foreach(echoI)
        val bandConstraints0 = c0si.map(compiler.compileQualified(_)).toSeq

        new Band(bandConstraints0)
      }

      Problem(cstate, bands)
    }

  def addConstraints(x: X, tag: Any = null): Unit = {
    val i = asg.getTreeRoot(x)
    val vars = asg.descendants(i).collect { case x @ InputF(_, _) => x }
    println(s"Variables in $tag")
    vars.foreach(v => println(s"  $v : ${v.typ}"))
    API.echo(asg.rootedAt(x), screenWidth = 50)

    addConstraintsHeadTails(x, tag)
  }

  def addConstraintsHeadTails(x: X, tag: Any = null): Unit = {
//    val i = headTails.getTreeRoot(x)
//    headTails.internalCoalgebra(i) match {
//      case ComputationF(CombinedErrors, errors, _) =>
//        errors.foreach { e =>
//          addSingleConstraint(e)
//        }
//      case _ => addSingleConstraint(i)
//    }
//    val vars = headTails.descendants(i).collect { case x @ InputF(_, _) => x }
//    println(s"Variables (head-tails) in $tag")
//    vars.map(v => s"  $v : ${v.typ}").sorted.foreach(println)
//    API.echo(headTails.rootedAt(x))
  }
//  def addSingleConstraint(e: I): Unit = {
//    println("Constraint: ")
//    println("  " + headTails.build(e))
//    val vars = headTails.descendants(e).collect { case x @ InputF(_, _) => x }
//    println(s"  variables (head-tails) ")
//    vars.map(v => s"    $v : ${v.typ}").sorted.foreach(println)
//    constraints += e
//  }
//  val insideView = OpenASG.ana(headTails.internalCoalgebra)
//  def solve(): RMemory = {
//    val mem = new MemImpl()
//
//    val exprs = constraints.map(i => RefExprs.asRefExpr(mem, i, insideView))
//    val ls = new LeastSquares(exprs.toList())
//    ls.lmIteration(mem, 100)
//
//    mem
//  }

//  type DS = I
//  type CS = I
//  type T = I
//  type Band = (DS, Seq[CS])
//  def create(): CS = ???
//
//  trait CSGen {
//    def apply(bandNum: Int, stateNum: Int): CS
//  }
//
//  def cstates(genS: CSGen, genT: () => T)(discretes: Seq[DS], num: Int): Seq[(DS, Seq[CS])] = {
//    val first = genS(0, 0)
//    for(i <- discretes.indices) {
//      val first = if(i == 0) first
//    }
//    val (last, bands) = discretes.foldLeft((first, List[Band]())) {
//      case ((previous, bands), ds) =>
//        val lastOfThis = (1 to num).map(i => genS())
//        val band = (ds,
//                    Seq(
//                      TimedState(previous, genT()),
//                      TimedState(lastOfThis, genT())
//                    ))
//        (lastOfThis, bands :+ band)
//    }
//
//    Bands(bands, first, last)
//
//  }

}
sealed trait Qualifier
object Qualifier {
  case object Always extends Qualifier
  case object AtStart extends Qualifier
  case object AtEnd extends Qualifier

}
case class Constraint(qual: Qualifier, fun: CompiledFunBridge)

class Compiler[I](_coalg: I => ExprF[I], cstate: ProductTagAny) {
  val _tree = OpenASG.ana(_coalg)
  val tree = _tree.fixID

  val coalg = tree.internalCoalgebra _
  type ID = tree.ID

  def compileQualified(i: I): Constraint = {
    tree.getExt(i) match {
      case ComputationF(ForAll, Vec(a), _) =>
        val compiled = compileLambda(a)
        Constraint(Qualifier.Always, compiled)
      case ComputationF(ForFirst, Vec(a), _) =>
        val compiled = compileLambda(a)
        Constraint(Qualifier.AtStart, compiled)

      case ComputationF(ForLast, Vec(a), _) =>
        val compiled = compileLambda(a)
        Constraint(Qualifier.AtEnd, compiled)

      case _ => // TODO: qualifier is currently optional
        val compiled = compileLambda(tree.getTreeRoot(i))
        Constraint(Qualifier.Always, compiled)
    }
  }
  def compileLambda(i: ID): CompiledFunBridge = {
    evaluation.compile[ID](coalg, cstate)(i)
  }
}
