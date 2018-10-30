package dahu.model.problem

import cats._
import cats.implicits._
import dahu.graphs._
import dahu.model.compiler.Algebras
import dahu.model.input.{Expr, Ident, TypedIdent}
import dahu.model.ir.{ExprF, StaticF, Total}
import dahu.model.problem.StaticProblem.Export
import dahu.utils.SFunctor

import scala.reflect.ClassTag
import scala.util.Try

object API {

  def echo[F[X] <: ExprF[X]: TreeNode: SFunctor](tree: RootedASG[_, F, cats.Id],
                                                 screenWidth: Int = 120): Unit = {
    println(tree.cata(Algebras.printAlgebraTree).mkString(screenWidth))
  }

  def parse(expr: Expr[Any]): LazyTree[Expr[_], ExprF, Id, _] =
    parse(expr, Algebras.coalgebra).forceEvaluation

  def parse[K, F[_]: SFunctor: TreeNode](root: K, coalgebra: K => F[K]): LazyTree[K, F, Id, _] =
    LazyTree.parse(root, coalgebra).forceEvaluation

  def eliminateDynamics[K](tree: RootedASG[K, ExprF, Id],
                           exports: Seq[Export[K]]): RootedASG[K, StaticF, Id] =
    StaticProblem
      .closeTheWorld[K](tree, exports)
      .postpro(dahu.model.transformations.makeOptimizer(dahu.model.transformations.totalPasses))
      .forceEvaluation

  def expandLambdas[K](tree: RootedASG[K, StaticF, Id]): RootedASG[K, Total, Id] =
    ExpandLambdas.expandLambdas[K](tree).forceEvaluation

  def expandLambdasThroughPartialEval[K](
      tree: RootedASG[K, StaticF, Id]): RootedASG[K, Total, Id] = {
    val graph = tree.fixID
    import dahu.model.transformations._
    val firstOrderExpansions = makeOptimizer(Pass.allStaticPasses)
    val expandedFirstOrder = graph.postpro(makeOptimizer(totalPasses)).postpro(firstOrderExpansions)
//    println("------ BEFORE -------")
//    echo(expandedFirstOrder)

    val t = expandedFirstOrder.tree.asInstanceOf[ASG[K, ExprF, Id]]

    val tPE: ASG[K, ExprF, Id] = t.manualMap(PartialEval)
    val tpeCasted = tPE.asInstanceOf[ASG[K, Total, Id]]

    val x = tpeCasted.rootedAt(tree.root).forceEvaluation

//    println("------ AFTER -------")
//    echo(x)

//    println("----- OPTIM ------")
    val y = optimize(x)
//    echo(y)

//    sys.exit(0)
    y
  }

  // TODO: make sure transformations are applied until the fix point.
  def optimize[K](tree: RootedASG[K, Total, Id]): RootedASG[K, Total, Id] =
    tree
      .postpro(dahu.model.transformations.optimizer)
      .postpro(dahu.model.transformations.optimizer)
      .postpro(dahu.model.transformations.optimizer)
      .forceEvaluation

  def parseAndProcess[K](root: K,
                         coalgebra: K => ExprF[K],
                         exports: Seq[Export[K]]): RootedASG[K, Total, Id] = {
    val parsed = parse(root, coalgebra)
    val noDynamics = eliminateDynamics[K](parsed, exports)
    val noLambdas = expandLambdas[K](noDynamics)
    optimize(noLambdas)
  }

  def parseAndProcess(expr: Expr[Any],
                      exports: Seq[Export[Expr[Any]]]): RootedASG[Expr[Any], Total, Id] =
    parseAndProcess(expr, Algebras.coalgebra, exports)

}
