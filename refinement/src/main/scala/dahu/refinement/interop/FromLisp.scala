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
import dahu.model.problem.API
import dahu.model.products.{Field, ProductTagAny}
import dahu.model.types._
import dahu.recursion.FAlgebra
import dahu.refinement.common.{Params, R, Values}
import dahu.refinement.la.LeastSquares
import dahu.refinement.{MemImpl, RMemory, RefExpr}
import dahu.utils._

import scala.util.Try

object FromLisp extends App {

  val env = sources.default()
  implicit val ctx = new Context(env, Some(transformations.scalarize))

//  evalFile("refinement/domains/prelude.clj")

  dahu.lisp.compile.evalMany(sources.prelude)
  dahu.lisp.compile.evalMany(sources.testDomain)

//  dahu.lisp.compile.evalFile("refinement/domains/car.clj")
//
//  sys.exit(0)

  val mem = new MemImpl()

  val _withParser = ctx.optGraph.fixID.flatPrepro[String](str => Try(dahu.lisp.parse(str)))

  val _asg = _withParser.transform(transformations.asErrors)
  val asg = _asg.fixID

  def asRefExpr[X, I <: IDTop, Opt[_]: Functor](x: X,
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

  def refexpr(str: String): Try[RefExpr] = {
    val tmp = asRefExpr(str, asg)
//    println(tmp)
    tmp
  }

  val x = mem
  x.print()

  implicit class RefExprHelper(val sc: StringContext) extends AnyVal {
    def str: String = sc.parts.mkString("")
    def e(args: Any*): Try[RefExpr] = refexpr(sc.parts.mkString(""))

    def eval2(args: Any*): Try[Value] =
      asg.getTreeRoot(str).map { i =>
        evaluation.eval(asg.internalCoalgebra, (id: TypedIdent) => Value(x.read(x.addressOf(id))))(
          evaluation.EvalEnv.empty())(i)
      }
  }

  val simplified = OpenASG.ana(asg.internalCoalgebra)

  val problem = new ContinuousProblem[asg.ID](simplified)

  def enforce(source: String): Unit = {
    asg.getTreeRoot(source) match {
      case scala.util.Success(value) =>
        problem.addConstraints(value, tag = source)
      case scala.util.Failure(e) => throw e
    }
  }

  enforce("(geom.is-in c1 pt1)")
  enforce("(geom.is-in c1 pt2)")
  enforce("(geom.is-in c2 pt3)")
  enforce("(geom.is-not-in r1 pt1)")
  enforce("(geom.is-in r2 pt3)")
  enforce("(= (:x (first path)) 0.0)")
  enforce("(= (:x (last path)) 10.0)")

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

object transformations {

  private val epsilon = 1e-8

  val scalarize = new Transformation[ExprF, ExprF] {
    override def transformation[I <: Int](retrieve: I => ExprF[I],
                                          record: ExprF[I] => I): ExprF[I] => ExprF[I] = {
      case InputF(id, tpe: ProductTagAny) =>
        val fields = tpe.fields.map {
          case Field(name, tpe, pos) =>
            val fieldId = TypedIdent(id.id.subIdent(name), tpe)
            record(InputF(fieldId, tpe))
        }
        ProductF(fields, tpe)

      case x => x
    }
  }

  val asErrors = new Transformation[ExprF, ExprF] {
    override def transformation[I <: Int](retrieve: I => ExprF[I],
                                          record: ExprF[I] => I): ExprF[I] => ExprF[I] = {
      case ComputationF(bool.And, args, _) =>
        ComputationF(double.Add, args)
      case ComputationF(double.LEQ, Vec(a, b), _) =>
        val bMinusA = record(
          ComputationF(double.Add, Vec(b, record(ComputationF(double.Negate, Vec(a))))))
        ComputationF(double.Min, record(CstF(Value(0.0), Tag.ofDouble)), bMinusA)
      case ComputationF(double.LT, Vec(a, b), _) =>
        val bMinusA = record(
          ComputationF(double.Add,
                       b,
                       record(ComputationF(double.Negate, Vec(a))),
                       record(CstF(Value(-epsilon), Tag.ofDouble)))
        )
        ComputationF(double.Min, record(CstF(Value(0.0), Tag.ofDouble)), bMinusA)
      case ComputationF(double.EQ, Vec(a, b), _) =>
        ComputationF(double.Add, a, record(ComputationF(double.Negate, b)))
      case ComputationF(bool.Or, _, _)  => ???
      case ComputationF(bool.Not, _, _) => ???
      case x                            => x
    }
  }

  val withHeadTail = new Transformation[ExprF, ExprF] {
    override def transformation[I <: Int](retrieve: I => ExprF[I],
                                          record: ExprF[I] => I): ExprF[I] => ExprF[I] = {
      case InputF(id, tpe: SequenceTagAny) =>
        val headIdent = TypedIdent(id.id.subIdent("first"), tpe.memberTag)
        val lastIdent = TypedIdent(id.id.subIdent("last"), tpe.memberTag)
        val head = record(InputF(headIdent))
        val tail = record(InputF(lastIdent))
        SequenceF(Vec(head, tail), tpe)

      case ComputationF(_: sequence.First[_], Vec(seq), _) =>
        retrieve(seq) match {
          case SequenceF(members, _) =>
            require(members.size > 0)
            retrieve(members.firstUnsafe)
        }

      case ComputationF(_: sequence.Last[_], Vec(seq), _) =>
        retrieve(seq) match {
          case SequenceF(members, _) =>
            require(members.size > 0)
            retrieve(members.lastUnsafe)
        }

      case x => x
    }
  }

  val optimizer =
    dahu.model.transformations.makeOptimizer[ExprF](dahu.model.transformations.Pass.allStaticPasses)
}

import transformations._

class ContinuousProblem[X](_asg: ASG[X, ExprF, Id]) {
  private val asg = _asg.fixID

  val headTails = asg
    .transform(withHeadTail)
    .transform(scalarize)
    .transform(optimizer)
    .transform(asErrors)
    .transform(optimizer)
    .fixID

  private val constraints = debox.Buffer[(X, Any)]()

  def addConstraints(x: X, tag: Any = null): Unit = {
    val i = asg.getTreeRoot(x)
    val vars = asg.descendants(i).collect { case x @ InputF(_, _) => x }
    println(s"Variables in $tag")
    vars.foreach(v => println(s"  $v : ${v.typ}"))

    addConstraintsHeadTails(x, tag)
  }

  def addConstraintsHeadTails(x: X, tag: Any = null): Unit = {
    val i = headTails.getTreeRoot(x)
    val vars = headTails.descendants(i).collect { case x @ InputF(_, _) => x }
    println(s"Variables (head-tails) in $tag")
    vars.foreach(v => println(s"  $v : ${v.typ}"))
    API.echo(headTails.rootedAt(x))
  }

}
