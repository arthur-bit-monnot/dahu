package dahu.refinement.interop

import cats.Functor
import cats.implicits._
import dahu.graphs.{IDTop, OpenASG, RootedASG}
import dahu.model.functions.{->:, Fun2}
import dahu.model.input.Lambda.LambdaIdent
import dahu.model.input.{Cst, Expr, Lambda}
import dahu.model.ir.{ExprF, LambdaF, ProductF}
import dahu.model.problem.API
import dahu.model.products.RecordType
import dahu.model.types.{LambdaTag, LambdaTagAny, Tag, TagAny}
import dahu.refinement.RMemory

object Test extends App {
  implicit val doubleTag = Tag.ofDouble
  implicit val doubleArrayTag = Tag.default[Array[Double]]

  type Addr = Int
  val AddrTag = new Tag[Addr] {
    override def clazz: _root_.dahu.utils.ClassTag[_root_.dahu.refinement.interop.Test.Addr] = ???
    override def typ: _root_.dahu.model.types.Tag.Type = ???
  }

  import dahu.model.input.dsl._

  val Point = RecordType("Point", Tag.ofDouble -> "x", Tag.ofDouble -> "y")
  type Point = ProductF[Any]

  val dist: Expr[Double ->: Double ->: Double] = Lambda(x => Lambda(y => x + y - Cst(10.0)))

  val parsed = API.parseAndProcess(dist, Nil)
  API.echo(parsed)

  val t = parsed.tree.fixID

  def arity(l: TagAny): Int = l match {
    case l: LambdaTagAny => 1 + arity(l.outType)
    case _               => 0
  }

  def arity[K, F[X] <: ExprF[X], Opt[_]: Functor](rooted: RootedASG[K, F, Opt]): Opt[Int] = {
    val t = rooted.tree.fixID
    val x = t.getTreeRoot(rooted.root).map(i => t.internalCoalgebra(i))
    x.map((fi: ExprF[_]) => arity(fi.typ))
  }

  def paramsOfTree[K, F[X] <: ExprF[X], Opt[_]: Functor](
      rooted: RootedASG[K, F, Opt]): Opt[List[LambdaIdent]] = {
    val t = rooted.tree.fixID
    val x = t.getTreeRoot(rooted.root)
    x.map(i => params(i, t))
  }

  def params[I <: IDTop, F[X] <: ExprF[X], Opt[_]](
      i: I,
      asg: OpenASG[_, F, Opt, I]): List[LambdaIdent] = {
    asg.internalCoalgebra(i) match {
      case LambdaF(in, tree, param, tpe) =>
        param :: params(tree, asg)
      case _ => Nil
    }
  }

  println(parsed.fixID.ofRoot.typ)

  println(parsed.fixID.nodes.mkString("\n"))

  println(arity(parsed))
  println(paramsOfTree(parsed)) //.mkString((i: LambdaIdent) => s"$i : ${i.}"))

}
