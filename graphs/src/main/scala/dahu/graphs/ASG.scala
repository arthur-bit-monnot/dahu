package dahu.graphs

import cats._
import cats.implicits._
import dahu.graphs.impl.{ASGTransformWithSubstitution, LazyForestGenerator}
import dahu.graphs.transformations._
import dahu.utils.{BiMap, ClassTag, SFunctor, SubSubInt}

/** Abstract Syntax Graph */
trait ASG[K, F[_], Opt[_]] {
  type ID <: IDTop

  sealed trait Marker

  def prepro[L](f: L => K): ASG[L, F, Opt]
  def flatPrepro[L](f: L => Opt[K])(implicit m: Monad[Opt]): ASG[L, F, Opt]

  def rootedAt(root: K): RootedASG[K, F, Opt] = RootedASG(root, this)

  def fixID: OpenASG[K, F, Opt, SubSubInt[IDTop, Marker]]

  def castIDTo[NewInternalID <: IDTop]: OpenASG[K, F, Opt, NewInternalID] =
    this.asInstanceOf[OpenASG[K, F, Opt, NewInternalID]]

  def transform[I <: IDTop, G[_]](fGen: Transformation[F, G])(
      implicit TN: TreeNode[F],
      F: Functor[Opt],
      SF: SFunctor[F],
      ct: ClassTag[G[I]]
  ): ASG[K, G, Opt]

  def transformWithSubstitution[G[_]](transformation: TransformationWithSubstitution[F, G])(
      implicit tn: TreeNode[F],
      ff: SFunctor[F],
      fg: SFunctor[G],
      fo: Functor[Opt],
      ct: ClassTag[G[IDTop]]): ASG[K, G, Opt] =
    ASGTransformWithSubstitution.build(this)(transformation)

  def manualMap[G[_]](mt: ManualTransformation[F, G])(
      implicit fo: Functor[Opt],
      ct: ClassTag[G[IDTop]]
  ): ASG[K, G, Opt] = {

    val t = this.fixID
    new OpenASG[K, G, Opt, IDTop] {
      private val coalg = BiMap[ID, G[ID]]()

      val ctx = new ManualTransformation.Context[F, G, t.ID, ID] {
        override def oget(i: t.ID): F[t.ID] = t.internalCoalgebra(i)
        override def nget(j: ID): G[ID] = coalg.get(j)
        override def nrec(gj: G[ID]): ID = {
          if(!coalg.cocontains(gj)) {
            val j = coalg.size.asInstanceOf[ID]
            coalg.add(j, gj)
          }
          coalg.coget(gj)
        }
      }
      val trans: t.ID => ID = mt.trans(ctx)

      override def getTreeRoot(k: K): Opt[ID] =
        t.getTreeRoot(k).map(trans)

      override def internalCoalgebra(i: ID): G[ID] = coalg.get(i)
    }
  }
}

object ASG {

  def ana[K, F[_]: SFunctor: TreeNode](coalgebra: K => F[K]): ASG[K, F, cats.Id] = {
    def algebra(ctx: LazyForestGenerator.Context[F, IDTop]): F[IDTop] => IDTop = ctx.record
    new LazyForestGenerator[K, F, F, cats.Id, IDTop](coalgebra, algebra)
  }
}
