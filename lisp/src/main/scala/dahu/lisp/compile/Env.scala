package dahu.lisp.compile

import java.io.File

import dahu.lisp._
import dahu.model.ir.{CstF, ExprF, InputF, ProductF}
import dahu.recursion.Fix
import dahu.utils.BiMap
import dahu.utils._

import scala.collection.mutable
import Env._
import cats.Id
import dahu.graphs.autotrans.AutoTransformation
import dahu.graphs.{ASG, ExtensibleASG, IDTop}
import dahu.model.compiler.Algebras
import dahu.model.compiler.Algebras.StringTree
import dahu.model.functions.{Fun, FunAny}
import dahu.model.input.TypedIdent
import dahu.model.math.bool
import dahu.model.products.{Constructor, FieldAccessAny, ProductTagAny, RecordType}
import dahu.model.types.SequenceTag.SequenceTagImplAny
import fastparse.core.Parsed.Success

import scala.io.Source
import scala.util.Try

trait Env[I <: Int] {
  type V = ExprF[I]
  def data: dahu.graphs.autotrans.AutoTransformation.Aux[ExprF, I]

//  protected def graph = ASG.ana[I, ExprF]((i: I) => data.get(i))
//  protected def print = graph.fixID.cata[StringTree](Algebras.printAlgebraTree)

  def subEnv(sym: Sym, value: I): Env[I] = {
    val sub = new SubEnv(this)
    sub.setConstantValue(sym.name, value)
    sub
  }
  def subEnv(params: Array[Sym], values: Array[I]): Env[I] = {
    assert(params.length == values.length)
    params.zip(values).foldLeft(this) { case (e, (p, v)) => e.subEnv(p, v) }
  }

  def getExistingCallSite(a: Sym): Option[I]
  def extractValue(a: Sym): V
  def extractValue(a: I): V
  def getValue(a: Sym): I
  def getId(e: V): I
  def setConstantValue(a: String, value: I): Unit

}

class RootEnv[I <: Int](base: dahu.graphs.autotrans.AutoTransformation.Aux[ExprF, I])
    extends Env[I] {

  private val index = mutable.Map[Sym, I]()
  val data = base
//  val data: BiMap[I, ExprF[I]] = BiMap()

//  private lazy val graph = ASG.ana[I, ExprF]((i: I) => data.get(i))
//  private lazy val print = graph.fixID.cata[StringTree](Algebras.printAlgebraTree)

//  def pprint(i: I): String = print.get(i).mkString(80)

//  private val data: mutable.Map[Sym, V] = mutable.Map()

  def getExistingCallSite(a: Sym): Option[I] = index.get(a)

  def getValue(a: Sym): I =
    getExistingCallSite(a)
      .getOrElse(error(s"Unknown symbol: $a"))

  def extractValue(a: Sym): V = extractValue(getValue(a))
  def extractValue(a: I): V = data.extract(a)

  def getId(e: V): I = data.deepRecord(e)

  def setConstantValue(a: String, value: I): Unit = {
    if(index.contains(Sym(a)))
      println(s"WARNING: redifining $a")
    index(Sym(a)) = value
  }

}

class SubEnv[I <: Int](val parent: Env[I]) extends Env[I] {

  private val index = mutable.Map[Sym, I]()
  def data = parent.data

  def getExistingCallSite(a: Sym): Option[I] = {
    index.get(a).orElse(parent.getExistingCallSite(a))
  }

  def getValue(a: Sym): I =
    getExistingCallSite(a)
      .getOrElse(error(s"Unknown symbol: $a"))

  def extractValue(a: Sym): V = extractValue(getValue(a))
  def extractValue(a: I): V = data.extract(a)

  def getId(e: V): I = data.deepRecord(e)

  def setConstantValue(a: String, value: I): Unit = {
    if(index.contains(Sym(a)))
      println(s"WARNING: redifining $a")
    index(Sym(a)) = value
  }

}

object Env {

  type DefaultID <: IDTop

  import dahu.model.types._
  import dahu.model.math._

  def default(): RootEnv[DefaultID] =
    default[DefaultID](AutoTransformation.empty[ExprF, DefaultID]())

  def default[I <: Int](base: AutoTransformation.Aux[ExprF, I]): RootEnv[I] = {
    val e = new RootEnv(base)

    def recVal(name: String, value: e.V): Unit = {
      val i = e.getId(value)
      e.setConstantValue(name, i)
    }
    def rec(name: String, f: FunAny): Unit = {
      val i = e.getId(CstF(Value(f), Tag.unsafe.ofAny))
      e.setConstantValue(name, i)
    }
    def recType(name: String, f: TagAny): Unit = {
      require(name.startsWith("^"))
      val i = e.getId(CstF(Value(f), Tag.unsafe.ofAny))
      e.setConstantValue(name, i)
    }
    rec("and", bool.And)
    rec("or", bool.Or)
    rec("not", bool.Not)
    rec("i+", int.Add)
    rec("+", double.Add)
    rec("i*", int.Times)
    rec("*", double.Times)
    rec("/", double.Div)
    rec("<", double.LT)
    rec("<=", double.LEQ)
    rec("neg", double.Negate)
    rec("imin", int.Min)
    rec("min", double.Min)
    rec("=", any.EQ[Any])
//    rec("read", ReadDouble)

    recVal("true", bool.TrueF)
    recVal("false", bool.FalseF)

    recType("^int", Tag.ofInt)
    recType("^real", Tag.ofDouble)
    recType("^bool", Tag.ofBoolean)
    recType("^str", Tag.ofString)
    e
  }

  object ops {
    implicit class EnvOps[I <: IDTop](val env: Env[I]) extends AnyVal {
      type V = env.V

      def TRUE = env.getId(dahu.model.math.bool.TrueF)
      def FALSE = env.getId(dahu.model.math.bool.FalseF)

      def typeOf(i: I): Type = env.extractValue(i).typ
      def record(v: V): I = env.getId(v)
      def get(i: I): V = env.extractValue(i)
      def extractTypedFieldsDefinitions(l: List[Any]): List[(String, TagAny)] = l match {
        case Nil => Nil
        case (fieldType: Sym) :: (fieldName: Sym) :: rest =>
          env.extractValue(fieldType) match {
            case CstF(t: TagAny, _) =>
              (fieldName.name, t) :: extractTypedFieldsDefinitions(rest)
            case x =>
              error(s"$fieldType does not resolve to a type but to: $x")
          }
        case _ => error(s"Expected a type and a field name but got: $l")
      }
      def input(id: TypedIdent): I = {
        record(InputF(id))
      }
      def cstString(v: String): I = {
        record(CstF(Value(v), Tag.ofString))
      }

      def defineStruct(recordName: String, fields: (String, TagAny)*): (RecordType, I) = {
        val r = RecordType(recordName, fields: _*)
        (r, defineStruct(r))
      }

      def defineStruct(r: ProductTagAny): I = {
        val recordName = r.name
        val fields = r.fields.map(f => f.name -> f.tag).toSeq
        log.verbose(s"Recording variable '^$recordName' representing a record type '$recordName'")
        val tpeId = record(CstF(Value(r), Tag.ofType))
        env.setConstantValue(s"^$recordName", tpeId)

        log.verbose(
          s"Recording variable '^${recordName}s' representing a record type '$recordName'")
        val listR = SequenceTagImplAny(r)
        val listTypeId = record(CstF(Value(listR), Tag.ofType))
        env.setConstantValue(s"^${recordName}s", listTypeId)

        log.verbose(s"Recording constructor '$recordName' for a record type")
        val ctor = Constructor(r)
        val ctorId = record(CstF(Value(ctor), ctor.funType))
        env.setConstantValue(recordName, ctorId)
        for(((fieldName, fieldType), i) <- fields.zipWithIndex) {
          val accessorName = s"$recordName.$fieldName"
          log.verbose(s"Recording getter $accessorName")
          val getter = new FieldAccessAny {
            override def arity: Option[Int] = Some(1)
            override def prodTag: Type = r
            override def fieldTag: Type = fieldType
            override def fieldPosition: Int = i
            override def name: String = accessorName
            override def compute(args: Vec[Value]): Any = args match {
              case Vec(ProductF(members, tpe)) if tpe == r =>
                members(i)
              case x => error(s"invalid argument to getter $accessorName: $x")
            }
            override def outType: Type = fieldType
            override def funType: LambdaTagAny = LambdaTag.of(prodTag, fieldTag)
          }
          val getterId = record(CstF(Value(getter), getter.funType))
          env.setConstantValue(accessorName, getterId)
        }
        tpeId
      }

      def parse(sourceCode: String): Try[I] =
        Try(dahu.lisp.parse(sourceCode))
          .map(sexpr => dahu.lisp.compile.eval(sexpr, env)(PartialEvalMode))

      def parseMany(sourceCode: String): Try[I] = {
        dahu.lisp.parser.parseMany(sourceCode) match {
          case Success(sexprs, _) =>
            Try {
              val trys = sexprs.toList.map(dahu.lisp.compile.eval(_, env)(PartialEvalMode))
              trys.last
            }
        }
      }

      def parseFile(filename: String): Try[I] = {
        println(s"Parsing $filename")
        dahu.lisp.parser.parseFile(new File(filename)) match {
          case Success(sexprs, _) =>
            Try {
              val trys = sexprs.toList.map(dahu.lisp.compile.eval(_, env)(PartialEvalMode))
              trys.last
            }

        }
      }
    }
  }

}
