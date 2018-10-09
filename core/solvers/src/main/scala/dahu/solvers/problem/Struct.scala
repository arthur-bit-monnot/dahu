package dahu.solvers.problem

import cats.Id
import dahu.graphs.TreeNode
import dahu.model.compiler.Algebras
import dahu.model.functions.->:
import dahu.model.input._
import dahu.model.math.bool
import dahu.model.types.{Bool, ProductTag, Tag}
import dahu.model.input.dsl._
import dahu.model.ir.{ExprF, Total}
import dahu.model.problem.{API, ASG, IDTop, LazyTree, OpenASG, SatisfactionProblem, StaticProblem}
import dahu.model.products.FieldAccess
import dahu.utils.SFunctor

import scala.collection.mutable.{ArrayBuffer => Buff}

case class CExpr[+A](e: Expr[A], ctx: Scope)

trait Struct {
  def scope: Scope
  final def prez: Expr[Bool] = scope.present
  def vars: Seq[Expr[Any]]
  def constraints: Seq[CExpr[Bool]]
  def subs: Seq[Struct]
  def exports: Seq[CExpr[Any]]

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append("Vars:\n")
    for(e <- vars) {
      sb.append("  ")
      e match {
        case Input(TypedIdent(id, typ)) =>
          sb.append(id + " :" + typ.typ + " [" + id.scope + "]")
        case _ =>
          sb.append(e)
      }
      sb.append("\n")
    }
    sb.append("Constraints:\n")
    for(CExpr(e, c) <- constraints) {
      sb.append("  ")
      sb.append(e)
      sb.append(" [")
      sb.append(c)
      sb.append("]\n")
    }
    sb.append("Exports:\n")
    for(CExpr(e, c) <- exports) {
      sb.append("  ")
      sb.append(e)
      sb.append(" [")
      sb.append(c)
      sb.append("]\n")
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
      for(e <- s.exports) in.exports += e
    }
    val s = new Structure(scope)
    mergeVarsAndConstraints(this, s)
    for(sub <- subs) {
      s.addConstraint(sub.prez ==> prez)
      mergeVarsAndConstraints(sub.flattened, s)
    }
    s.seal()
  }
}

case class EncodedProblem[+Res](asg: ASG[Expr[Any], Total, cats.Id],
                                sat: Expr[Bool],
                                res: Expr[Res])

object Struct {

  def echo[F[X] <: ExprF[X]: TreeNode: SFunctor](tree: LazyTree[_, F, cats.Id, _]): Unit = {
    println(tree.cata(Algebras.printAlgebraTree).mkString(120))
  }

  def encode[T](flat: Struct, result: Expr[T]): EncodedProblem[T] = {
    assert(flat.subs.isEmpty)
    val constraints: Seq[Expr[Bool]] = flat.constraints.map {
      case CExpr(c, scope) => scope.present ==> c
    }
    val pb: Expr[Bool] = bool.And(constraints: _*)
    val dynAsg = API.parse(pb).fixID

    val exported = flat.exports.map {
      case CExpr(e, ctx) => (e, ctx.present)
    }
    val staticAsg = StaticProblem.closeTheWorld(dynAsg, exported)
    val noLambdas = API.expandLambdas(staticAsg).fixID
    val optTree = noLambdas.postpro(SatisfactionProblem.Optimizations.optimizer)

    EncodedProblem(optTree.tree, pb, result)
  }

  def process(flat: Struct) = {
    val constraints: Seq[Expr[Bool]] = flat.constraints.map {
      case CExpr(c, scope) => scope.present ==> c
    }
    val pb: Expr[Bool] = bool.And(constraints: _*)
    val dynAsg = API.parse(pb).fixID
    println(dynAsg.fullTree)
    for(exp <- flat.exports) {
      println("AA")
      println(dynAsg.tree.getExt(exp.e))
    }

    println("-------------------- Static")
    val exported = flat.exports.map {
      case CExpr(e, ctx) => (e, ctx.present)
    }
    val staticAsg = StaticProblem.closeTheWorld(dynAsg, exported)

    echo(staticAsg)
//    println(staticAsg.fullTree)
//    println(staticAsg.tree.cata(Algebras.printAlgebraTree).get(staticAsg.root).mkString(120))

    println("-------------- No lambdas")
    val noLambdas = API.expandLambdas(staticAsg).fixID
    echo(noLambdas)
//    println(noLambdas.fullTree)

    println("-------------- reduced")
    //  val optGraph = noLambdas.tree.transform(SatisfactionProblem.Optimizations.optimizer)
    val optTree = noLambdas.postpro(SatisfactionProblem.Optimizations.optimizer) //LazyTree(optGraph)(noLambdas.root)
    echo(optTree)
//    println(optTree.fullTree)

  }
}

final class Structure(val scope: Scope) extends Struct {

  var isSealed: Boolean = false
  val vars: Buff[Expr[Any]] = Buff()
  val constraints: Buff[CExpr[Bool]] = Buff()
  val subs: Buff[Struct] = Buff()
  val exports: Buff[CExpr[Any]] = Buff()

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
  def addConstraint(c: Expr[Bool]): CExpr[Bool] = {
    assert(!isSealed)
    val cc = CExpr[Bool](c, scope)
    constraints += cc
    cc
  }

  def addSub(name: String): Structure = {
    assert(!isSealed)
    val subPrez = addVar[Bool](s"$name?")
    val subContext = scope.subScope(name, subPrez)
    val s = new Structure(subContext)
    subs += s
    s
  }

  def addExport(e: Expr[Any]): CExpr[Any] = {
    assert(!isSealed)
    val ce = CExpr(e, scope)
    exports += ce
    ce
  }
}

object Structure {
  def newRoot(): Structure = new Structure(Scope.root)
}

object Test extends App {
  import dsl._

  case class Interval[F[_]](start: F[Int], end: F[Int])
  object Interval {
    implicit val tag: Tag[Interval[Id]] = ProductTag.ofProd[Interval]
    val Start = FieldAccess[Interval, Int]("start", 0)
    val End = FieldAccess[Interval, Int]("end", 1)

    val IsValid: Expr[Interval[Id] ->: Bool] = Lambda(itv => Start(itv) <= End(itv))
  }
  case class Point[F[_]](pt: F[Int])
  object Point {
    implicit val tag: Tag[Point[Id]] = ProductTag.ofProd[Point]
    val Value = FieldAccess[Point, Int]("pt", 0)

    val IsValid: Expr[Point[Id] ->: Bool] = Lambda(pt => Value(pt) >= 10)
  }

  val csp = Structure.newRoot()
  val a = csp.addVar[Bool]("a")
  val b = csp.addVar[Bool]("b")

  val x = csp.addSub("X")
  val xa = x.addVar[Int]("a")
  val xb = x.addVar[Int]("b")
  x.addConstraint(xa =!= xb)
  x.addExport(Product(Interval(xa, xb)))
  x.addExport(Product(Point(xa)))

  csp.addConstraint(a <= b)

  println(csp)

  println("----------------")

  val flat = csp.flattened

  val constraints: Seq[Expr[Bool]] = flat.constraints.map {
    case CExpr(c, scope) => scope.present ==> c
  }
  val pb: Expr[Bool] = bool.And(constraints: _*)
  val dynAsg = API.parse(pb).fixID
  println(dynAsg.fullTree)
  for(exp <- flat.exports) {
    println("AA")
    println(dynAsg.tree.getExt(exp.e))
  }

  println("-------------------- Static")
  val exported = flat.exports.map {
    case CExpr(e, ctx) => (e, ctx.present)
  }
  val staticAsg = StaticProblem.closeTheWorld(dynAsg, exported)

  println(staticAsg.fullTree)

  println("-------------- No lambdas")
  val noLambdas = API.expandLambdas(staticAsg).fixID
  println(noLambdas.fullTree)

  println("-------------- reduced")
  //  val optGraph = noLambdas.tree.transform(SatisfactionProblem.Optimizations.optimizer)
  val optTree = noLambdas.postpro(SatisfactionProblem.Optimizations.optimizer) //LazyTree(optGraph)(noLambdas.root)
  println(optTree.fullTree)
}
