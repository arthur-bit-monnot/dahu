package dahu.graphs
import cats.Functor
import dahu.graphs.impl.ASGTransformWithSubstitution
import dahu.graphs.transformations._
import dahu.utils.{ClassTag, SFunctor, SubSubInt}

/** Abstract Syntax Graph */
trait ASG[K, F[_], Opt[_]] {
  type ID <: IDTop

  sealed trait Marker

  def rootedAt(root: K): RootedASG[K, F, Opt] = RootedASG(root, this)

  def fixID: OpenASG[K, F, Opt, SubSubInt[IDTop, Marker]]

  def castIDTo[NewInternalID <: IDTop]: OpenASG[K, F, Opt, NewInternalID] =
    this.asInstanceOf[OpenASG[K, F, Opt, NewInternalID]]

  def transformWithSubstitution[G[_]](transformation: TransformationWithSubstitution[F, G])(
      implicit tn: TreeNode[F],
      ff: SFunctor[F],
      fg: SFunctor[G],
      fo: Functor[Opt],
      ct: ClassTag[G[IDTop]]): ASG[K, G, Opt] =
    ASGTransformWithSubstitution.build(this)(transformation)
}
