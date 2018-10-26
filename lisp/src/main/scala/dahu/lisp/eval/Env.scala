package dahu.lisp.eval

import java.lang.invoke._

import dahu.lisp._

import scala.collection.mutable

class Env(val parent: Option[Env] = None) {
  val tpe = MethodType.methodType(classOf[AnyRef])

  private val data: mutable.Map[Sym, CallSite] = mutable.Map()

  def subEnv(sym: Sym, value: AnyRef): Env = {
    val sub = new Env(Some(this))
    //    println(s"registering: $sym -> $value")
    sub.setConstantValue(sym.name, value)
    sub
  }
  def subEnv(params: Array[Sym], values: Array[AnyRef]): Env = {
    assert(params.length == values.length)
    params.zip(values).foldLeft(this) { case (e, (p, v)) => e.subEnv(p, v) }
  }

  private def getExistingCallSite(a: Sym): Option[CallSite] = {
    data.get(a).orElse(parent.flatMap(_.getExistingCallSite(a)))
  }

  def getValue(a: Sym): AnyRef =
    getExistingCallSite(a)
      .getOrElse(error(s"Unknown symbol: $a"))
      .dynamicInvoker()
      .invoke()

  private def getOrCreateLocalCallSite(a: Sym) = {
    if(!data.contains(a)) {
      val cs = new MutableCallSite(tpe)
      data(a) = cs
    }
    data(a)
  }

  def setConstantValue(a: String, value: Any): Unit = {
    val mh = MethodHandles.constant(classOf[AnyRef], value)
    getOrCreateLocalCallSite(Sym(a)).setTarget(mh)
  }
  def setHandle(a: String, value: MethodHandle): Unit = {
    getOrCreateLocalCallSite(Sym(a)).setTarget(value)
  }
  def setFunctionHandle(a: String, value: MethodHandle): Unit = {
//    println(s"Recording function $a: ${value.`type`()}")
    setConstantValue(a, value)
    //      getCallSite(Sym(a)).setTarget(value)
  }
}

object Env {

  private case class Crash(msg: String) extends Exception

  def default(): Env = {
    // TODO: default should be immutalble and return a child
    val env = new Env(parent = None)

    env.setConstantValue("true", true)
    env.setConstantValue("false", false)
    env.setFunctionHandle("+", mh.mh((a: Int, b: Int) => a + b))
    env.setFunctionHandle("*", mh.mh((a: Int, b: Int) => a * b))
    env.setFunctionHandle("<", mh.mh((a: Int, b: Int) => a < b))
    env.setFunctionHandle(">", mh.mh((a: Int, b: Int) => a > b))
    env.setFunctionHandle("<=", mh.mh((a: Int, b: Int) => a <= b))
    env.setFunctionHandle("=", mh.mh((a: AnyRef, b: AnyRef) => a == b))
    env.setFunctionHandle("and", mh.mh((a: Boolean, b: Boolean) => a && b))
    env.setFunctionHandle("or", mh.mh((a: Boolean, b: Boolean) => a || b))
    env.setFunctionHandle("first", mh.mh((a: List[AnyRef]) => a.head))
    env.setFunctionHandle("rest", mh.mh((a: List[AnyRef]) => a.tail))
    env.setFunctionHandle("cons", mh.mh((a: AnyRef, l: List[AnyRef]) => a :: l))
    env.setFunctionHandle("print", mh.mhVarArgs((a: Array[AnyRef]) => {
      println(a.map(format).mkString(" ")); a
    }))
    env.setFunctionHandle("printerr", mh.mhVarArgs((a: Array[AnyRef]) => {
      System.err.println(a.map(format).mkString(" ")); a
    }))
    env.setFunctionHandle("crash!!", mh.mhVarArgs((a: Array[AnyRef]) => {
      throw Crash(a.map(format).mkString(" ")); a
    }))

    env.setFunctionHandle("map",
                          mh.mh((f: MethodHandle, l: List[AnyRef]) => l.map(a => f.invoke(a))))
    env.setFunctionHandle("foldl",
                          mh.mh((f: MethodHandle, acc: AnyRef, l: List[AnyRef]) =>
                            l.foldLeft(acc)((acc, a) => f.invoke(acc, a))))
    env
  }
}
