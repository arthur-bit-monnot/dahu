package dahu.lisp.compile

import dahu.lisp._
import dahu.model.ir.{CstF, ExprF}
import dahu.recursion.Fix
import dahu.utils.BiMap
import dahu.utils._

import scala.collection.mutable
import Env._
import dahu.graphs.{ASG, IDTop}
import dahu.model.compiler.Algebras
import dahu.model.compiler.Algebras.StringTree
import dahu.model.functions.{Fun, FunAny}
import dahu.model.math.bool

trait Env {
  def data: BiMap[I, ExprF[I]]

//  protected def graph = ASG.ana[I, ExprF]((i: I) => data.get(i))
//  protected def print = graph.fixID.cata[StringTree](Algebras.printAlgebraTree)

  def subEnv(sym: Sym, value: I): Env = {
    val sub = new SubEnv(this)
    sub.setConstantValue(sym.name, value)
    sub
  }
  def subEnv(params: Array[Sym], values: Array[I]): Env = {
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

class RootEnv extends Env {

  private val index = mutable.Map[Sym, I]()
  val data: BiMap[I, ExprF[I]] = BiMap()

  private lazy val graph = ASG.ana[I, ExprF]((i: I) => data.get(i))
  private lazy val print = graph.fixID.cata[StringTree](Algebras.printAlgebraTree)

  def pprint(i: I): String = print.get(i).mkString(80)

//  private val data: mutable.Map[Sym, V] = mutable.Map()

  def getExistingCallSite(a: Sym): Option[I] = index.get(a)

  def getValue(a: Sym): I =
    getExistingCallSite(a)
      .getOrElse(error(s"Unknown symbol: $a"))

  def extractValue(a: Sym): V = extractValue(getValue(a))
  def extractValue(a: I): V = data.get(a)

  def getId(e: V): I = {
    if(!data.cocontains(e)) {
      val i: I = data.size.asInstanceOf[I]
      data.add(i, e)
      assert(data.coget(e) == i)
    }
    data.coget(e)
  }

  def setConstantValue(a: String, value: I): Unit = {
    if(index.contains(Sym(a)))
      println(s"WARNING: redifining $a")
    index(Sym(a)) = value
  }

}

class SubEnv(val parent: Env) extends Env {

  private val index = mutable.Map[Sym, I]()
  def data: BiMap[I, ExprF[I]] = parent.data

  def getExistingCallSite(a: Sym): Option[I] = {
    index.get(a).orElse(parent.getExistingCallSite(a))
  }

  def getValue(a: Sym): I =
    getExistingCallSite(a)
      .getOrElse(error(s"Unknown symbol: $a"))

  def extractValue(a: Sym): V = extractValue(getValue(a))
  def extractValue(a: I): V = data.get(a)

  def getId(e: V): I = {
    if(!data.cocontains(e)) {
      val i: I = data.size.asInstanceOf[I]
      data.add(i, e)
      assert(data.coget(e) == i)
    }
    data.coget(e)
  }

  def setConstantValue(a: String, value: I): Unit = {
    if(index.contains(Sym(a)))
      println(s"WARNING: redifining $a")
    index(Sym(a)) = value
  }

}

object Env {
  type I <: IDTop
  type V = ExprF[I]

  import dahu.model.types._
  import dahu.model.math._
  def default(): RootEnv = {
    val e = new RootEnv()

    def recVal(name: String, value: V): Unit = {
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

}
