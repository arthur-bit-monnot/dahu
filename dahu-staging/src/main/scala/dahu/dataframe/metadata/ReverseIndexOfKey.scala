package dahu.dataframe.metadata

import dahu.dataframe.utils.ReverseIndexOf
import shapeless._
import shapeless.ops.nat.ToInt

trait ReverseIndexOfKey[K, M <: HList] {
  type Out <: Nat
  def apply(): Int
}
object ReverseIndexOfKey {
  type Aux[K, M <: HList, N <: Nat] = ReverseIndexOfKey[K, M] { type Out = N }

  def apply[K, M <: HList](implicit instance: ReverseIndexOfKey[K, M]) =
    instance

  implicit def indexOfKey[K, M <: HList, CM, N <: Nat](
      implicit columMetadata: ColumnMeta.Aux[K, M, CM],
      index: ReverseIndexOf.Aux[CM, M, N],
      toInt: ToInt[N]): ReverseIndexOfKey.Aux[K, M, N] =
    new ReverseIndexOfKey[K, M] {
      override type Out = N
      override def apply(): Int = toInt()
    }

}
