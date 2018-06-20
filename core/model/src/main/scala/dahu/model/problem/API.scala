package dahu.model.problem

import cats._
import cats.implicits._
import dahu.graphs.TreeNode
import dahu.model.compiler.Algebras
import dahu.model.input.{Expr, Ident, TypedIdent}
import dahu.model.interpreter.{Interpreter, LambdaInterpreter, PEval, Result}
import dahu.model.ir.{ExprF, NoApplyF, StaticF, Total}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.types._
import dahu.utils.SFunctor
import dahu.recursion._

import scala.reflect.ClassTag
import scala.util.Try

object API {

  def parse(expr: Expr[Any]): LazyTree[Expr[_], ExprF, Id, _] =
    parse(expr, Algebras.coalgebra).forceEvaluation

  def parse[K, F[_]: SFunctor: TreeNode](root: K, coalgebra: K => F[K]): LazyTree[K, F, Id, _] =
    LazyTree.parse(root, coalgebra).forceEvaluation

  def eliminitateDynamics[K](tree: LazyTree[K, ExprF, Id, _]): LazyTree[K, StaticF, Id, _] =
    StaticProblem.underClosedWorld[K](tree).forceEvaluation

  def expandLambdas[K](tree: LazyTree[K, StaticF, Id, _]): LazyTree[K, NoApplyF, Id, _] =
    ExpandLambdas.expandLambdas[K](tree).forceEvaluation

  def makeTotal[K](t: LazyTree[K, NoApplyF, Id, _]): LazyTree[K, Total, IR, _] = {
    SatisfactionProblem.encode(t).forceEvaluation
  }

  def parseAndProcess[K](root: K, coalgebra: K => ExprF[K]): LazyTree[K, Total, IR, _] = {
    val parsed = parse(root, coalgebra)
    val noDynamics = eliminitateDynamics[K](parsed)
    val noLambdas = expandLambdas[K](noDynamics)
    makeTotal(noLambdas)
  }

  @deprecated("intended for debug only", since = "forever")
  def parseAndProcessPrint(e: Expr[_]): LazyTree[Expr[_], Total, IR, _] = {
    val nodes = Expr.dagInstance.descendantsAndSelf(e)
    def printAll[F[_]: SFunctor, Opt[_]: Functor](tree: LazyTree[Expr[_], F, Opt, _])(
        implicit ev: ClassTag[F[Fix[F]]]): Unit = {
      nodes.foreach(n =>
        Try {
          val ast = tree.tree.getTreeRoot(n).map(i => tree.tree.build(i))
          println(s"${tree.tree.getTreeRoot(n)}  $n")
          println("  " + ast)
      })
    }

    val parsed = parse(e, Algebras.coalgebra)
    println("\nParsed")
    parsed.fullTree
    printAll[ExprF, Id](parsed)
    val noDynamics = eliminitateDynamics[Expr[_]](parsed)
    println("\nno dynamics")
    noDynamics.fullTree
    printAll[StaticF, Id](noDynamics)
    val noLambdas = expandLambdas[Expr[_]](noDynamics)
    println("no-lambdas")
    noLambdas.fullTree
    printAll[NoApplyF, Id](noLambdas)
    val total = makeTotal(noLambdas)
    println("\nTotal")
    val x = total.mapExternal[Id](_.valid).fullTree
    println(x)
    printAll(total)

//    val intBool = new IntBoolSatisfactionProblem(total.mapExternal[Id](_.value)).tree
//    println("\nint bool")
//    println(intBool.fullTree)
//    printAll(intBool)

    total
  }

  def parseAndProcess(expr: Expr[_]): LazyTree[Expr[_], Total, IR, _] =
    parseAndProcess(expr, Algebras.coalgebra)

  def eval[T](expr: Expr[T], inputs: TypedIdent[Any] => Value): PEval[T] =
    parse(expr).noDynamics.expandLambdas
      .eval[PEval[Value]](LambdaInterpreter.partialEvalAlgebra(inputs.andThen(Some(_))))
      .asInstanceOf[PEval[T]]

  def evalTotal(expr: Expr[_], inputs: TypedIdent[Any] => Value): IR[PEval[Any]] =
    API.parseAndProcess(expr).eval(Interpreter.evalAlgebra(inputs.andThen(Some(_))))

  implicit class NoDynamicOps[K](private val tree: LazyTree[K, ExprF, Id, _]) extends AnyVal {
    def noDynamics: LazyTree[K, StaticF, Id, _] = eliminitateDynamics(tree)
  }
  implicit class ExpandLambdasOps[K](private val tree: LazyTree[K, StaticF, Id, _]) extends AnyVal {
    def expandLambdas: LazyTree[K, NoApplyF, Id, _] =
      API.expandLambdas(tree)
  }
  implicit class MakeTotalOps[K](private val tree: LazyTree[K, NoApplyF, Id, _]) extends AnyVal {
    def totalSubParts: LazyTree[K, Total, IR, _] = API.makeTotal(tree)
  }

}
