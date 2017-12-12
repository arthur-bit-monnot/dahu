package dahu.dataframe.metadata

import dahu.dataframe.DataFrame
import shapeless.<:!<

/**
  * Given that the data frame as column X, with key CM.K returns a new dataframe metadata with X replaced by CM
  */
trait Swapped[CM <: ColMeta, MD <: FrameMeta] {
  type Out <: FrameMeta

  def apply[K, V, F[_]](df: DataFrame[MD], newColMeta: CM, newValues: F[V])(
      implicit ev: CM <:< ColMeta.Aux[K, V, F]): DataFrame[Out]
}

object Swapped {
  type Aux[CM <: ColMeta, MD <: FrameMeta, Out0 <: FrameMeta] =
    Swapped[CM, MD] { type Out = Out0 }

  /** Head of metadata has the same key, switch it */
  implicit def columnTypeOfHead[K, CM <: ColMeta, H <: ColMeta, T <: FrameMeta](
      implicit ev: CM <:< ColMeta.KAux[K],
      ev2: H <:< ColMeta.KAux[K]): Swapped.Aux[CM, H ::: T, CM ::: T] = {
    new Swapped[CM, H ::: T] {
      override type Out = CM ::: T

      override def apply[K, V, F[_]](df: DataFrame[H ::: T],
                                     newColMeta: CM,
                                     newValues: F[V])(
          implicit ev: CM <:< ColMeta.Aux[K, V, F]): DataFrame[Out] = {
        DataFrame[CM ::: T](
          newColMeta ::: df.meta.tail,
          df.cols.updated(df.meta.size - 1, newValues)
        )
      }
    }
  }

  implicit def columnTypeOfOthers[K,
                                  CM <: ColMeta,
                                  H <: ColMeta,
                                  T <: FrameMeta,
                                  TOut <: FrameMeta](
      implicit tailSwap: Swapped.Aux[CM, T, TOut],
      ev2: H <:!< ColMeta.KAux[K]): Swapped.Aux[CM, H ::: T, H ::: TOut] = {
    new Swapped[CM, H ::: T] {
      override type Out = H ::: TOut

      override def apply[K, V, F[_]](df: DataFrame[:::[H, T]],
                                     newColMeta: CM,
                                     newValues: F[V])(
          implicit ev: <:<[CM, ColMeta.Aux[K, V, F]]): DataFrame[H ::: TOut] = {
        val DataFrame(metaTail, columns) = tailSwap.apply(
          DataFrame(df.meta.tail, df.cols),
          newColMeta,
          newValues)
        DataFrame[H ::: TOut](df.meta.head ::: metaTail, columns)
      }

    }
  }
}
