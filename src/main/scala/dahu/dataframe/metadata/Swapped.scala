package dahu.dataframe.metadata

import dahu.dataframe.DataFrame
import shapeless.<:!<

/**
  * Given that the data frame as column X, with key CM.K returns a new dataframe metadata with X replaced by CM
  */
trait Swapped[K, V, F[_], CM <: ColMeta.Aux[K, V, F], MD <: FrameMeta] {
  type Out <: FrameMeta

  def apply(df: DataFrame[MD], newColMeta: CM, newValues: F[V]): DataFrame[Out]
}

object Swapped {
  type Aux[K,
           V,
           F[_],
           CM <: ColMeta.Aux[K, V, F],
           MD <: FrameMeta,
           Out0 <: FrameMeta] =
    Swapped[K, V, F, CM, MD] { type Out = Out0 }

  /** Head of metadata has the same key, switch it */
  implicit def swappedOfHead[K,
                             V,
                             F[_],
                             CM <: ColMeta.Aux[K, V, F],
                             H <: ColMeta,
                             T <: FrameMeta](
      implicit ev2: H <:< ColMeta.KAux[K])
    : Swapped.Aux[K, V, F, CM, H ::: T, CM ::: T] = {
    new Swapped[K, V, F, CM, H ::: T] {
      override type Out = CM ::: T

      override def apply(df: DataFrame[H ::: T],
                         newColMeta: CM,
                         newValues: F[V]): DataFrame[Out] = {
        DataFrame[CM ::: T](
          newColMeta ::: df.meta.tail,
          df.cols.updated(df.meta.size - 1, newValues)
        )
      }
    }
  }

  implicit def swappedOfOthers[K,
                               V,
                               F[_],
                               CM <: ColMeta.Aux[K,V,F],
                               H <: ColMeta,
                               T <: FrameMeta,
                               TOut <: FrameMeta](
      implicit tailSwap: Swapped.Aux[K,V,F,CM, T, TOut],
      ev2: H <:!< ColMeta.KAux[K]): Swapped.Aux[K,V,F,CM, H ::: T, H ::: TOut] = {
    new Swapped[K,V,F,CM, H ::: T] {
      override type Out = H ::: TOut

      override def apply(df: DataFrame[:::[H, T]],
                                     newColMeta: CM,
                                     newValues: F[V]): DataFrame[H ::: TOut] = {
        val DataFrame(metaTail, columns) = tailSwap.apply(
          DataFrame(df.meta.tail, df.cols),
          newColMeta,
          newValues)
        DataFrame[H ::: TOut](df.meta.head ::: metaTail, columns)
      }

    }
  }
}
