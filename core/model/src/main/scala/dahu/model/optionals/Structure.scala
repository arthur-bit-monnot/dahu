package dahu.model.optionals
import dahu.model.input._

import dahu.model.math.bool
import dahu.model.types.Tag
import dahu.model.input.dsl._

import scala.collection.mutable.{ArrayBuffer => Buff}

trait Context {
  def id: String
  def prez: Expr[Boolean]

  def subId(localId: String): String = id match {
    case "" => localId
    case x  => x + "." + localId
  }

  override def toString: String = "@" + id
}
case object RootContext extends Context {
  override def id = ""
  override def prez: Expr[Boolean] = bool.True
}
case class SubContext(name: String, parent: Context) extends Context {
  override val id: String = parent.subId(name)
  override val prez: Expr[Boolean] =
    new Input[Boolean](TypedIdent(Ident(subId("prez?")), Tag[Boolean]))
}

case class CExpr[+A](e: Expr[A], ctx: Context)

trait Struct {
  def context: Context
  final def prez: Expr[Boolean] = context.prez
  def vars: Seq[CExpr[Any]]
  def constraints: Seq[CExpr[Boolean]]
  def subs: Seq[Struct]

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append("Vars\n")
    for(CExpr(e, c) <- vars) {
      sb.append("  ")
      sb.append(e)
      sb.append(" ")
      sb.append(c)
      sb.append("\n")
    }
    sb.append("Constraints:\n")
    for(CExpr(e, c) <- constraints) {
      sb.append("  ")
      sb.append(e)
      sb.append(" ")
      sb.append(c)
      sb.append("\n")
    }

    for(sub <- subs) {
      sb.append("\nSub: " + sub.context + "\n")
      sb.append(sub.toString)
    }
    sb.toString()
  }

  def flattened: Struct = {
    def mergeVarsAndConstraints(s: Struct, in: Structure): Unit = {
      for(v <- s.vars) in.vars += v
      for(c <- s.constraints) in.constraints += c
    }
    val s = new Structure(context)
    mergeVarsAndConstraints(this, s)
    for(sub <- subs) {
      mergeVarsAndConstraints(sub.flattened, s)
      s.addConstraint(sub.prez ==> prez)
    }
    s.seal()
  }
}

final class Structure(val context: Context) extends Struct {

  var isSealed: Boolean = false
  val vars: Buff[CExpr[Any]] = Buff()
  val constraints: Buff[CExpr[Boolean]] = Buff()
  val subs: Buff[Struct] = Buff()

  def seal(): Struct = { isSealed = true; this }

  def addVar[T: Tag](name: String): CExpr[T] = {
    assert(!isSealed)
    val in = new Input[T](TypedIdent(Ident(context.subId(name)), Tag[T]))
    val v = CExpr(in, context)
    vars += v
    v
  }
  def addVar[T](e: Expr[T]): CExpr[T] = {
    assert(!isSealed)
    val v = CExpr(e, context)
    vars += v
    v
  }
  def addConstraint(c: Expr[Boolean]): CExpr[Boolean] = {
    assert(!isSealed)
    val cc = CExpr[Boolean](c, context)
    constraints += cc
    cc
  }

  def addSub(name: String): Structure = {
    val subContext = SubContext(name, context)
    addVar(subContext.prez)
    val s = new Structure(subContext)
    subs += s
    s
  }
}

object Structure {
  def newRoot(): Structure = new Structure(RootContext)
}

object Test extends App {
  import dsl._

  val csp = Structure.newRoot()
  val a = csp.addVar[Boolean]("a")
  val b = csp.addVar[Boolean]("b")

  val x = csp.addSub("X")
  x.addVar[Int]("a")

  csp.addConstraint(a.e <= b.e)

  println(csp)

  println("----------------")

  println(csp.flattened)
}
