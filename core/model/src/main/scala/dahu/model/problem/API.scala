package dahu.model.problem

import cats._
import cats.implicits._
import dahu.graphs.TreeNode
import dahu.model.compiler.Algebras
import dahu.model.input.{Expr, Ident}
import dahu.model.interpreter.Interpreter
import dahu.model.ir.{ExprF, NoApplyF, StaticF, Total}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types._
import dahu.utils.SFunctor
import dahu.recursion._

import scala.reflect.ClassTag

object API {

  def parse(expr: Expr[_]): LazyTree[Expr[_], ExprF, Id, _] = parse(expr, Algebras.coalgebra)

  def parse[K, F[_]: SFunctor: TreeNode](root: K, coalgebra: K => F[K]): LazyTree[K, F, Id, _] =
    LazyTree.parse(root, coalgebra)

  def eliminitateDynamics[K](tree: LazyTree[K, ExprF, Id, _]): LazyTree[K, StaticF, Id, _] =
    StaticProblem.underClosedWorld[K](tree)

  def expandLambdas[K, Opt[_]: Functor](
      tree: LazyTree[K, StaticF, Opt, _]): LazyTree[K, NoApplyF, Opt, _] =
    ExpandLambdas.expandLambdas[K, Opt](tree)

  def makeTotal[K](t: LazyTree[K, NoApplyF, Id, _]): LazyTree[K, Total, IR, _] = {
    SatisfactionProblem.encode(t)
  }

  def parseAndProcess[K](root: K, coalgebra: K => ExprF[K]): LazyTree[K, Total, IR, _] = {
    val parsed = parse(root, coalgebra)
    val noDynamics = eliminitateDynamics[K](parsed)
    val noLambdas = expandLambdas[K, Id](noDynamics)
    makeTotal(noLambdas)
  }

  @deprecated("intended for debug only", since = "forever")
  def parseAndProcessPrint(e: Expr[_]): LazyTree[Expr[_], Total, IR, _] = {
    val nodes = Expr.dagInstance.descendantsAndSelf(e)
    def printAll[F[_]: SFunctor, Opt[_]: Functor](tree: LazyTree[Expr[_], F, Opt, _])(
        implicit ev: ClassTag[F[Fix[F]]]): Unit = {
      nodes.foreach(n => {
        val ast = tree.tree.getTreeRoot(n).map(i => tree.tree.build(i))
        println(s"${tree.tree.getTreeRoot(n)}  $n")
        println("  " + ast)
      })
    }

    val parsed = parse(e, Algebras.coalgebra)
    println("\nParsed")
    println(parsed.fullTree)
//    printAll[ExprF, Id](parsed)
    val noDynamics = eliminitateDynamics[Expr[_]](parsed)
    println("\nno dynamics")
    println(noDynamics.fullTree)
//    printAll[StaticF, Id](noDynamics)
    val noLambdas = expandLambdas[Expr[_], Id](noDynamics)
    println(noLambdas.fullTree)
    val total = makeTotal(noLambdas)
    println("\nTotal")
    println(total.fullTree)
//    printAll(total)

//    val intBool = new IntBoolSatisfactionProblem(total.mapExternal[Id](_.value)).tree
//    println("\nint bool")
//    println(intBool.fullTree)
//    printAll(intBool)

    total
  }

  def parseAndProcess(expr: Expr[_]): LazyTree[Expr[_], Total, IR, _] =
    parseAndProcess(expr, Algebras.coalgebra)

  def eval[T](expr: Expr[T], inputs: Ident => Value): Interpreter.Result[T] =
    parse(expr).noDynamics.expandLambdas
      .eval[Interpreter.Result[Value]](Interpreter.partialEvalAlgebra(inputs))
      .asInstanceOf[Interpreter.Result[T]]

  def evalTotal(expr: Expr[_], inputs: Ident => Value): IR[Value] =
    API.parseAndProcess(expr).eval(Interpreter.evalAlgebra(inputs))

  implicit class NoDynamicOps[K](private val tree: LazyTree[K, ExprF, Id, _]) extends AnyVal {
    def noDynamics: LazyTree[K, StaticF, Id, _] = eliminitateDynamics(tree)
  }
  implicit class ExpandLambdasOps[K, Opt[_]](private val tree: LazyTree[K, StaticF, Opt, _])
      extends AnyVal {
    def expandLambdas(implicit F: Functor[Opt]): LazyTree[K, NoApplyF, Opt, _] =
      API.expandLambdas(tree)
  }
  implicit class MakeTotalOps[K](private val tree: LazyTree[K, NoApplyF, Id, _]) extends AnyVal {
    def totalSubParts: LazyTree[K, Total, IR, _] = API.makeTotal(tree)
  }

}
