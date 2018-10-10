package dahu.graphs

import cats._
import cats.implicits._
import dahu.recursion.Fix
import dahu.utils.{ClassTag, SFunctor, SubSubInt}

trait RootedASG[K, F[_], Opt[_]] { self: LazyTree[K, F, Opt, _] =>
  val tree: ASG[K, F, Opt]
  val root: K

  def fixID: LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]] =
    this.asInstanceOf[LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]]]

  def forceEvaluation: RootedASG[K, F, Opt]

  def postpro(fGen: (SomeID => F[SomeID], F[SomeID] => SomeID) => (F[SomeID] => F[SomeID]))(
      implicit TN: TreeNode[F],
      F: Functor[Opt],
      SF: SFunctor[F],
      ct: ClassTag[F[SomeID]]
  ): RootedASG[K, F, Opt] = tree.transform(fGen).rootedAt(root)

  def cata[V: ClassTag](
      f: F[V] => V)(implicit T: TreeNode[F], F: SFunctor[F], FO: Functor[Opt]): Opt[V] =
    tree.cata(f).get(root)

  def fullTree(implicit F: SFunctor[F], FO: Functor[Opt], ct: ClassTag[F[Fix[F]]]): Opt[Fix[F]] =
    tree.getTreeRoot(root).map(tree.build)
}
object RootedASG {
  def apply[K, F[_], Opt[_]](root: K, asg: ASG[K, F, Opt]): RootedASG[K, F, Opt] =
    LazyTree[K, F, Opt, IDTop](asg.asInstanceOf[OpenASG[K, F, Opt, IDTop]])(root)
}
