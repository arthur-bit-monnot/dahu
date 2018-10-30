package dahu.lisp.compile

import dahu.lisp._
import dahu.model.ir.{CstF, ExprF}
import dahu.recursion.Fix
import dahu.utils.BiMap
import dahu.utils._

import scala.collection.mutable
import Env._
import dahu.graphs.ASG
import dahu.model.compiler.Algebras
import dahu.model.compiler.Algebras.StringTree
import dahu.model.functions.{Fun, FunAny}
import dahu.model.math.bool

class Env(val parent: Option[Env] = None) {

  private val index = mutable.Map[Sym, I]()
  private val data: BiMap[I, ExprF[I]] =
    parent match {
      case Some(p) => p.data
      case _       => BiMap()
    }

  private lazy val graph = ASG.ana[I, ExprF]((i: I) => data.get(i))
  private lazy val print = graph.fixID.cata[StringTree](Algebras.printAlgebraTree)

  def pprint(i: I): String = print.get(i).mkString(80)

//  private val data: mutable.Map[Sym, V] = mutable.Map()

  def subEnv(sym: Sym, value: I): Env = {
    val sub = new Env(Some(this))
    sub.setConstantValue(sym.name, value)
    sub
  }
  def subEnv(params: Array[Sym], values: Array[I]): Env = {
    assert(params.length == values.length)
    params.zip(values).foldLeft(this) { case (e, (p, v)) => e.subEnv(p, v) }
  }

  private def getExistingCallSite(a: Sym): Option[I] = {
    index.get(a).orElse(parent.flatMap(_.getExistingCallSite(a)))
  }

  def getValue(a: Sym): I =
    getExistingCallSite(a)
      .getOrElse(error(s"Unknown symbol: $a"))

  def extractValue(a: Sym): V = extractValue(getValue(a)) // TODO: should look in parents
  def extractValue(a: I): V = data.get(a) // TODO: should look in parents

  def getId(e: V): I = {
    if(!data.cocontains(e)) {
      val i: I = data.size.asInstanceOf[I]
      data.add(i, e)
      assert(data.coget(e) == i)
    }
    data.coget(e)
  }

  def setConstantValue(a: String, value: I): Unit = {
    index(Sym(a)) = value
  }

}

object Env {
  type I <: Int
  type V = ExprF[I]

  import dahu.model.types._
  import dahu.model.math._
  def default(): Env = {
    val e = new Env()

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
    rec("i+", int.Add)
    rec("+", double.Add)
    rec("i*", int.Times)
    rec("*", double.Times)
    rec("imin", int.Min)
    rec("min", double.Min)
    rec("=", any.EQ[Any])

    recType("^int", Tag.ofInt)
    recType("^real", Tag.ofDouble)
    recType("^str", Tag.ofString)
    e
  }

}