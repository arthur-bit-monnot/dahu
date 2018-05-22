package dahu.model.problem

import cats.{Functor, Id}
import dahu.graphs.TreeNode
import dahu.model.ir.{ExprF, NoApplyF, StaticF, Total}
import dahu.model.problem.SatisfactionProblem.IR
import dahu.utils.SFunctor

object API {

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

  def parseAndPreProcess[K](root: K, coalgebra: K => ExprF[K]): LazyTree[K, Total, IR, _] = {
    val parsed = parse(root, coalgebra)
    val noDynamics = eliminitateDynamics[K](parsed)
    val noLambdas = expandLambdas[K, Id](noDynamics)
    makeTotal(noLambdas)
  }

}
