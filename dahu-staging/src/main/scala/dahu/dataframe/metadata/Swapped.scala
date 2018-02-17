package dahu.dataframe.metadata

import dahu.dataframe.DF
import shapeless.{::, HList}

/**
  * Given that the data frame as column X, with the same key K as CM returns a new dataframe with X replaced by CM
  */
trait Swapped[K, CM, MD <: HList] {
  type Out <: HList

  def apply[V, F[_]](df: DF[MD], newColMeta: CM, newValues: F[V])(
      implicit value: Value.Aux[CM, V],
      container: Container.Aux[CM, F]): DF[Out]
}

object Swapped {
  type Aux[K, CM, MD <: HList, Out0 <: HList] =
    Swapped[K, CM, MD] { type Out = Out0 }

  /** Head of metadata has the same key, switch it */
  implicit def swappedOfHead[K, CM, H, T <: HList](
      implicit prevKey: Key.Aux[H, K],
      index: ReverseIndexOfKey[K, H :: T]): Swapped.Aux[K, CM, H :: T, CM :: T] = {
    new Swapped[K, CM, H :: T] {
      override type Out = CM :: T

      override def apply[V, F[_]](df: DF[H :: T], newColMeta: CM, newValues: F[V])(
          implicit value: Value.Aux[CM, V],
          container: Container.Aux[CM, F]
      ): DF[Out] = {
        DF[Out](
          df.cols.updated(index(), newValues)
        )
      }
    }
  }

  implicit def swappedOfOthers[K, CM, H, T <: HList, TOut <: HList](
      implicit tailSwap: Swapped.Aux[K, CM, T, TOut]): Swapped.Aux[K, CM, H :: T, H :: TOut] = {
    new Swapped[K, CM, H :: T] {
      override type Out = H :: TOut

      override def apply[V, F[_]](df: DF[H :: T], newColMeta: CM, newValues: F[V])(
          implicit value: Value.Aux[CM, V],
          container: Container.Aux[CM, F]): DF[H :: TOut] = {
        val DF(columns) = tailSwap.apply(DF(df.cols), newColMeta, newValues)
        DF[H :: TOut](columns)
      }
    }
  }
}
