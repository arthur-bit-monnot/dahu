package dahu.lisp

import java.io.File

import cats._
import cats.implicits._
import dahu.graphs.{ASG, IDTop, OpenASG, RootedASG}
import dahu.graphs.transformations.Transformation
import dahu.lisp.keywords._
import dahu.model.compiler.Algebras
import dahu.model.compiler.Algebras.StringTree
import dahu.model.functions.{Fun, FunAny}
import dahu.model.input.{Ident, Lambda, Scope, TypedIdent}
import dahu.model.ir._
import dahu.model.problem.API
import dahu.model.products._
import dahu.model.types.LambdaTag.LambdaTagImpl
import dahu.model.types.SequenceTag.SequenceTagImplAny
import dahu.model.types._
import dahu.recursion.Fix
import dahu.utils._
import fastparse.core.Parsed

import scala.io.Source
import scala.util.{Failure, Success, Try}

object log {
  val logLevel = 3

  def verbose(msg: String): Unit = if(logLevel >= 3) println(msg)
}

package object compile {

  val CURRENT_CONTINUOUS_STATE = "current-continuous-state"
//  val CURRENT_DISCRETE_STATE =
//    TypedIdent(Ident(Scope.root, "current-discrete-state"), Tag.ofInt)

  import Env._

  sealed trait EvalMode
  case object Quoted extends EvalMode
  case object PartialEvalMode extends EvalMode

  import Env.ops._

  val CUR_EVENTS = "current-events"
  val DSTATE = "current-discrete-state"

  def defineEvents[I <: IDTop](env: Env[I], r: ProductTagAny): I = {
    require(r.name == "events")
    val curEvs = TypedIdent(Ident(Scope.root, CUR_EVENTS), r)
    val iCurEvs = env.record(InputF(curEvs))
    env.setConstantValue(CUR_EVENTS, iCurEvs)
    env.setConstantValue("NO_EVENTS", env.record(ProductF(r.fields.map(_ => env.FALSE), r)))
    def readEvent(name: String): I = {
      val getter = r.getAccessorAny(name).get
      env.record(ComputationF(getter, iCurEvs))
    }
    r.fields.map(_.name).foreach(name => env.setConstantValue(name, readEvent(name)))
    env.defineStruct(r)
  }

  def defineDiscreteState[I <: IDTop](env: Env[I], r: ProductTagAny): I = {
    val curState = TypedIdent(Ident(Scope.root, DSTATE), r)
    val state = env.record(InputF(curState))
    env.setConstantValue(DSTATE, state)
    def readDisc(name: String): I = {
      val getter = r.getAccessorAny(name).get
      env.record(ComputationF(getter, state))
    }
    r.fields.map(_.name).foreach { name =>
      env.setConstantValue(name, readDisc(name))
    }
    env.defineStruct(r)
  }
  def defineContinuousState[I <: IDTop](env: Env[I], r: ProductTagAny): I = {
    require(r.fieldPosition("dt").nonEmpty, "Continuous state must have a 'dt' field")
    val curState = env.getValue(Sym(CURRENT_CONTINUOUS_STATE))
    val readContTpe = RecordType("read-cont", "state" -> Tag.ofInt, "field" -> Tag.ofString)
    def readCont(name: String): I = {
      env.record(ProductF(Vec(curState, env.cstString(name)), readContTpe))
    }

    for(Field(name, tpe, _) <- r.fields.toSeq) {
      require(tpe == Tag.ofDouble)
      env.setConstantValue(name, readCont(name))
    }
    env.defineStruct(r)
  }

  def eval[I <: IDTop](e: SExpr, env: Env[I])(implicit mode: EvalMode): I = {
    val TRUE = env.getId(dahu.model.math.bool.TrueF)
    val FALSE = env.getId(dahu.model.math.bool.FalseF)
    def typeOf(i: I): Type = env.typeOf(i)
    def record(v: env.V): I = env.record(v)
    def get(i: I): env.V = env.get(i)

    def cstString(v: String): I = {
      record(CstF(Value(v), Tag.ofString))
    }

    e match {
      case x: Sym =>
        env.getExistingCallSite(x) match {
          case Some(i) => i
          case None if x.name.startsWith(":") =>
            eval(List(Sym("get-field"), x.name.substring(1)), env)
//            env.getId(CstF.of(GetField(x.name.substring(1)), Tag.unsafe.ofAny))
          case None => error(s"Unknown symbol $x")
        }
      case x: Int    => env.getId(CstF(Value(x), Tag.ofInt))
      case x: Double => env.getId(CstF(Value(x), Tag.ofDouble))
      case x: String => env.getId(CstF(Value(x), Tag.ofString))
      case true      => TRUE
      case false     => FALSE
      case DO :: rest =>
        assert(rest.nonEmpty)
        rest.map(eval(_, env)).last
      case IF :: cond :: onTrue :: onFalse :: Nil =>
        val c = eval(cond, env)
        c match {
          case TRUE                            => eval(onTrue, env)
          case FALSE                           => eval(onFalse, env)
          case x if typeOf(x) != Tag.ofBoolean => error(s"Unexpected condition result: ${get(x)}")
          case _ =>
            val t = eval(onTrue, env)
            val f = eval(onFalse, env)
            env.getId(ITEF(c, t, f, typeOf(t)))

        }
      case QUOTE :: (e: List[_]) :: Nil =>
        record(
          SequenceF[I](
            e.map(eval(_, env)(Quoted)),
            SequenceTagImplAny(Tag.unsafe.ofAny)
          ))
//      case ATOM :: (_: List[_]) :: Nil => false
//      case ATOM :: _ :: Nil            => true
      case LAMBDA :: (args: Seq[Sym]) :: exp :: Nil =>
        def const(params: List[Sym], e: Env[I]): I = params match {
          case Nil => eval(exp, e)
          case head :: tail =>
            val p = LambdaParamF[I](Lambda.LambdaIdent(head.name), Tag.unsafe.ofAny)
            val subE = e.subEnv(head, record(p))
            val sub = const(tail, subE)
            val tpe = LambdaTag.of(p.typ, typeOf(sub))
            val lbd = LambdaF(record(p), sub, p.id, tpe)
            record(lbd)
        }
        const(args.toList, env)
      case DEFINE :: Sym(label) :: expr :: Nil =>
        val value = eval(expr, env)
        env.setConstantValue(label, value)
        value

      case Sym("defvar") :: rest =>
        rest match {
          case (tpe: Sym) :: Sym(name) :: Nil =>
            env.getExistingCallSite(tpe).map(get) match {
              case Some(CstF(tpe: TagAny, _)) =>
                val ident = TypedIdent(Ident(Scope.root, name), tpe)
                val i = record(InputF(ident, tpe))
                env.setConstantValue(name, i)
                i
              case None    => error(s"Unknown type: $tpe")
              case Some(x) => error(s"Expected type but got: $x")
            }
          case x => error("Malformed var definition")

        }
      case Sym("define-continuous-state") :: rest =>
        val fields = env.extractTypedFieldsDefinitions(rest) ++ List("dt" -> Tag.ofDouble)
        val cstateTpe = RecordType("cstate", fields: _*)
        defineContinuousState(env, cstateTpe)

      case Sym("define-discrete-state") :: rest =>
        val fields = env.extractTypedFieldsDefinitions(rest)
        val dstateTpe = RecordType("dstate", fields: _*)
        defineDiscreteState(env, dstateTpe)

      case Sym("define-events") :: rest =>
        assert(rest.forall(_.isInstanceOf[Sym]))
        val fields = rest.map(_.asInstanceOf[Sym].name).map((_, Tag.ofBoolean))
        val eventsTpe = RecordType("dstate", fields: _*)
        defineEvents(env, eventsTpe)

      case Sym("defstruct") :: Sym(recordName) :: rest =>
        val fields = env.extractTypedFieldsDefinitions(rest)
        env.defineStruct(recordName, fields: _*)._2

      case l: List[_] =>
        println(l)
        l.head match {
          case LAMBDA | DEFINE | QUOTE | IF => error(s"Malformed expression $l")
          case _                            =>
        }
        val fid :: args = l.map(eval(_, env))

        val argsKnown: Option[List[Value]] = args
          .map(get)
          .map {
            case CstF(v, _) => Some(v)
            case _          => None
          }
          .sequence

        val res = get(fid) match {
          case CstF(f: FunAny, _) =>
            (mode, argsKnown) match {
              case (PartialEvalMode, Some(vals)) =>
                val value = f.compute(Vec.fromSeq(vals))
                record(CstF(Value(value), f.outType))
              case _ =>
                record(ComputationF(f, args, f.outType))
            }

          case x =>
            args.foldLeft(fid) {
              case (lbd, a) =>
                typeOf(lbd) match {
                  case lt: LambdaTagAny =>
                    val e = ApplyF(lbd, a, lt.outType)
                    record(e)
                  case _ =>
                    // type is unknown at this point
                    val e = ApplyF(lbd, a, Tag.unsafe.ofAny)
                    record(e)
//                    error(s"Unexpected application to a non-lambda type: $realLambda :  $x")
                }

            }
        }
        println("  " + get(res))
        res

    }
  }

  val trans = new Transformation[ExprF, ExprF] {
    override def transformation[I <: Int](retrieve: I => ExprF[I],
                                          record: ExprF[I] => I): ExprF[I] => ExprF[I] = {
      val pEvaluator = new PartialEvaluator[I] {
        override def rec(exprF: ExprF[I]): I = record(exprF)
        override def ret(i: I): ExprF[I] = retrieve(i)
      }
      val x: I => I = pEvaluator.peval(Map())
      e: ExprF[I] =>
        retrieve(x(record(e)))
    }
  }

  abstract class PartialEvaluator[I <: Int] {
    def rec(exprF: ExprF[I]): I
    def ret(i: I): ExprF[I]
    def known(l: Seq[I]): Option[List[Value]] =
      l.toList
        .map(ret)
        .map {
          case CstF(v, _) => Some(v)
          case _          => None
        }
        .sequence
    def peval(e: Map[Lambda.LambdaIdent, I])(i: I): I = ret(i) match {
      case fi @ ComputationF(f, argsExprs, tpe) =>
        val args = argsExprs.map(peval(e)(_))
        known(args.toList) match {
          case Some(values) => rec(CstF(Value(f.compute(Vec.fromSeq(values))), f.outType))
          case None =>
            rec(ComputationF(f, args, tpe))
        }
      case ApplyF(lbdI, argI, tpe) =>
        val arg = peval(e)(argI)
        ret(lbdI) match {
          case LambdaF(in, tree, ident, _) =>
            peval(e.updated(ident, arg))(tree)
          case _ =>
            rec(ApplyF(lbdI, arg, tpe))
        }
      case LambdaParamF(id, tpe) =>
        e.getOrElse(id, i)
      case x =>
        rec(x.smap(peval(e)(_)))

    }
  }

  type LispStr = String

  def graphBuilder[I <: IDTop](env: RootEnv[I]): ASG[SExpr, ExprF, Try] = {
    new OpenASG[SExpr, ExprF, Try, I] {
      override def getTreeRoot(k: SExpr): Try[I] = {
        Try(eval(k, env)(Quoted))
      }
      override def internalCoalgebra(i: I): ExprF[I] =
        env.extractValue(i)
    }
  }

  class Context[I <: IDTop](env: RootEnv[I], trans: Option[Transformation[ExprF, ExprF]] = None) {

    val graph: ASG[SExpr, ExprF, Try] = graphBuilder(env)
    val transGraph = trans match {
      case Some(transformation) => graph.transform(transformation)
      case _                    => graph
    }
    val optGraph = {
      // TODO: we should not need to exapnd twice...
      val g1 = API.expandLambdasThroughPartialEval(transGraph)
      val g2 = API.expandLambdasThroughPartialEval(g1)
      API.expandLambdasThroughPartialEval(g2)
    }
    val pprint = optGraph.fixID.cata[StringTree](Algebras.printAlgebraTree)

    def show(raw: SExpr) = pprint.get(raw).map(_.mkString(40))

    def treeOf(raw: SExpr): Try[Fix[ExprF]] = optGraph.rootedAt(raw).fullTree

  }

  def evalToString[I <: IDTop](se: SExpr, ctx: Context[I]): Try[String] = {
    ctx.show(se)
  }

  def parseEvalToString[I <: IDTop](str: String, ctx: Context[I]): Try[String] = {
    for {
      sexpr <- Try(parse(str))
      res <- evalToString(sexpr, ctx)
    } yield res
  }

  def parseEval[I <: IDTop](str: String, ctx: Context[I]): Try[Fix[ExprF]] = {
    for {
      sexpr <- Try(parse(str))
      res <- ctx.treeOf(sexpr)
    } yield res
  }

  def format(e: Any): String = e match {
    case l: List[_] => l.map(format).mkString("(", " ", ")")
    case e          => e.toString
  }

  def evalMany[I <: IDTop](sourceCode: String)(implicit ctx: Context[I]): Unit = {
    val ast = dahu.lisp.parser.parseMany(sourceCode) match {
      case Parsed.Success(sexprs, _) => sexprs
      case x =>
        println("Failed to parse:")
        println(x)
        sys.exit(1)
    }
    for(e <- ast) {
      dahu.lisp.compile.evalToString(e, ctx) match {
        case Success(res) =>
          println(res)
        case Failure(e) =>
          e.printStackTrace()
          sys.exit(1)
      }
    }
  }

  def evalFile[I <: IDTop](filename: String)(implicit ctx: Context[I]): Unit = {
    println(s"Parsing $filename")
    val sourceCode = Source.fromFile(new File(filename)).getLines().fold("")((a, b) => a + "\n" + b)
    evalMany(sourceCode)
  }

}
