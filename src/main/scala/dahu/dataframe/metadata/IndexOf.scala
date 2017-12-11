package dahu.dataframe.metadata

import shapeless._
import shapeless.ops.nat.ToInt

trait IndexOf[K, M <: FrameMeta] {
  type Out <: Nat
  def apply(): Int
}
object IndexOf {
  type Aux[K, M <: FrameMeta, N <: Nat] = IndexOf[K, M] { type Out = N }

  def apply[K, M <: FrameMeta](implicit instance: IndexOf[K, M]) = instance

  implicit def indexOfHead[K, N <: Nat, H <: ColMeta.KAux[K], T <: FrameMeta](
      implicit ev1: T <:< FrameMeta.SizeAux[N],
      ev: ToInt[N]
  ): IndexOf.Aux[K, H ::: T, N] = {
    new IndexOf[K, H ::: T] {
      override type Out = N
      override def apply(): Int = ev()
    }
  }
  implicit def indexOfOthers[K, H <: ColMeta, T <: FrameMeta, N <: Nat](
      implicit ev: IndexOf.Aux[K, T, N],
      toInt: ToInt[N],
      ev2: H <:!< ColMeta.KAux[K]): IndexOf.Aux[K, H ::: T, N] = {
    new IndexOf[K, H ::: T] {
      override type Out = N
      override def apply(): Int = toInt()
    }
  }
}
