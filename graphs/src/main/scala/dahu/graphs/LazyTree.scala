package dahu.graphs

import cats._
import cats.implicits._
import dahu.graphs.impl.LazyForestGenerator
import dahu.utils._

class LazyTree[K, F[_], Opt[_], InternalID <: IDTop] private (
    val tree: OpenASG[K, F, Opt, InternalID],
    val root: K)
    extends RootedASG[K, F, Opt] {
  final type ID = InternalID

  override def fixID: LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]] =
    this.asInstanceOf[LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]]]

  def forceEvaluation: LazyTree[K, F, Opt, InternalID] = { tree.forceEvaluation(root); this }

  def mapK[G[_]](fk: F ~> G): LazyTree[K, G, Opt, ID] = map(a => fk(a))
  def map[G[_]](f: F[ID] => G[ID]): LazyTree[K, G, Opt, ID] =
    LazyTree(tree.mapInternal(f))(root)

  def ofRoot(implicit F: Functor[Opt]): Opt[F[InternalID]] =
    tree.getTreeRoot(root).map(i => tree.internalCoalgebra(i))

  def eval[V: ClassTag](
      f: F[V] => V)(implicit F: Functor[Opt], SF: SFunctor[F], T: TreeNode[F]): Opt[V] =
    tree.cata(f).get(root)

  def mapExternal[Opt2[_]](f: Opt[ID] => Opt2[ID]): LazyTree[K, F, Opt2, ID] =
    LazyTree(tree.mapExternal(f))(root)

  def nodes(implicit tn: TreeNode[F], F: Functor[Opt]): Opt[Seq[(tree.ID, F[tree.ID])]] =
    tree.getTreeRoot(root).map(tree.descendants)
}

object LazyTree {

  def apply[K, F[_], Opt[_], I <: IDTop](tree: OpenASG[K, F, Opt, I])(
      root: K): LazyTree[K, F, Opt, tree.ID] =
    new LazyTree[K, F, Opt, tree.ID](tree, root)

  def parse[K, F[_]: SFunctor: TreeNode](t: K, coalgebra: K => F[K]): LazyTree[K, F, cats.Id, _] = {
    def algebra(ctx: LazyForestGenerator.Context[F, IDTop]): F[IDTop] => IDTop = ctx.record
    val forest = new LazyForestGenerator[K, F, F, cats.Id, IDTop](coalgebra, algebra)
    LazyTree(forest)(t)
  }

  def parseGen[K, FIn[_]: SFunctor: TreeNode, FOut[_]](
      t: K,
      coalgebra: K => FIn[K],
      algebra: LazyForestGenerator.Context[FOut, IDTop] => FIn[IDTop] => IDTop) =
    //:  IlazyForest[K, FOut, cats.Id, _] = TODO: make opaque once IntBoolSatisfactionProblem is clean
    new LazyForestGenerator[K, FIn, FOut, cats.Id, IDTop](coalgebra, algebra)

}
