package dahu.dataframe

import dahu.dataframe.metadata.{ColMeta, ColumnMeta, FrameMeta, IndexOf}

trait Col[V, F[_], DF] {
  def values: F[V]
  def get(row: Int): V
  def updated(row: Int, value: V): DF
  def swapped(values: F[V]): DF
}

object Col {

  def extractColumn[K, V, F[_], M <: FrameMeta, CM <: ColMeta](df: DataFrame[M],
                                                         k: K)(
      implicit index: IndexOf[K, M],
      fieldType: ColumnMeta.Aux[K, M, CM],
      ev: CM <:< ColMeta.Aux[K, V, F]): Col[V, F, DataFrame[M]] =
    new Col[V, F, DataFrame[M]] {
      private val vec: Vec[F, V] = fieldType(df.meta).vec.asInstanceOf[Vec[F, V]]

      override def values: F[V] = df.cols(index()).asInstanceOf[F[V]]

      override def get(row: Int): V = vec.at(values, row)
      override def updated(row: Int, value: V): DataFrame[M] =
        new DataFrame[M](
          df.meta,
          df.cols.updated(index(), vec.updated(values, row, value)))

      override def swapped(values: F[V]): DataFrame[M] = {
        new DataFrame[M](df.meta, df.cols.updated(index(), values))
      }
    }

}
