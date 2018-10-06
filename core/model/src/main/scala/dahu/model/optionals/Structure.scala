package dahu.model.optionals
import dahu.model.input._

import dahu.model.math.bool
import dahu.model.types.Tag
import dahu.model.input.dsl._

import scala.collection.mutable.{ArrayBuffer => Buff}

case class CExpr[+A](e: Expr[A], ctx: Scope)

trait Struct {
  def scope: Scope
  final def prez: Expr[Boolean] = scope.present
  def vars: Seq[Expr[Any]]
  def constraints: Seq[CExpr[Boolean]]
  def subs: Seq[Struct]

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append("Vars\n")
    for(e <- vars) {
      sb.append("  ")
      e match {
        case Input(TypedIdent(id, typ)) =>
          sb.append(id.lid + " :" + typ.typ + " [" + id.scope + "]")
        case _ =>
          sb.append(e)
      }
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
      sb.append("\nSub: " + sub.scope + "\n")
      sb.append(sub.toString)
    }
    sb.toString()
  }

  def flattened: Struct = {
    def mergeVarsAndConstraints(s: Struct, in: Structure): Unit = {
      for(v <- s.vars) in.vars += v
      for(c <- s.constraints) in.constraints += c
    }
    val s = new Structure(scope)
    mergeVarsAndConstraints(this, s)
    for(sub <- subs) {
      mergeVarsAndConstraints(sub.flattened, s)
      s.addConstraint(sub.prez ==> prez)
    }
    s.seal()
  }
}

final class Structure(val scope: Scope) extends Struct {

  var isSealed: Boolean = false
  val vars: Buff[Expr[Any]] = Buff()
  val constraints: Buff[CExpr[Boolean]] = Buff()
  val subs: Buff[Struct] = Buff()

  def seal(): Struct = { isSealed = true; this }

  def addVar[T: Tag](name: String): Expr[T] = {
    assert(!isSealed) // todo: check existence
    val v = Input[T](name, scope)
    vars += v
    v
  }
  def addVar[T](v: Expr[T]): Expr[T] = {
    assert(!isSealed)
    vars += v
    v
  }
  def addConstraint(c: Expr[Boolean]): CExpr[Boolean] = {
    assert(!isSealed)
    val cc = CExpr[Boolean](c, scope)
    constraints += cc
    cc
  }

  def addSub(name: String): Structure = {
    val subPrez = addVar[Boolean](s"$name?")
    val subContext = scope.subScope(name, subPrez)
    val s = new Structure(subContext)
    subs += s
    s
  }
}

object Structure {
  def newRoot(): Structure = new Structure(Scope.root)
}

object Test extends App {
  import dsl._

  val csp = Structure.newRoot()
  val a = csp.addVar[Boolean]("a")
  val b = csp.addVar[Boolean]("b")

  val x = csp.addSub("X")
  x.addVar[Int]("a")

  csp.addConstraint(a <= b)

  println(csp)

  println("----------------")

  println(csp.flattened)
}
