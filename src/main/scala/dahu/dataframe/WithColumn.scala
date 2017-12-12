package dahu.dataframe

import dahu.dataframe.metadata.{ColMeta, ColumnMeta, FrameMeta, IndexOf}
import dahu.dataframe.vector.Vec

trait WithColumn[K, MD <: FrameMeta] {
  type V
  type F[_]

  def values(df: DataFrame[MD]): F[V]
  def get(df: DataFrame[MD], row: Int): V
  def updated(df: DataFrame[MD], row: Int, value: V): DataFrame[MD]
  def swapped(df: DataFrame[MD], values: F[V]): DataFrame[MD]
}

object WithColumn {
  type Aux[K, V0, F0[_], MD <: FrameMeta] = WithColumn[K, MD] { type V = V0; type F[T] = F0[T] }

  implicit def extractColumn[K, V0, F0[_], M <: FrameMeta, CM <: ColMeta](
      implicit index: IndexOf[K, M],
      fieldType: ColumnMeta.Aux[K, M, CM],
      ev: CM <:< ColMeta.Aux[K, V0, F0]): WithColumn.Aux[K, V0, F0, M] =
    new WithColumn[K, M] {
      override type V = V0
      override type F[T] = F0[T]

      private def vec(df: DataFrame[M]): Vec[F, V] =
        fieldType(df.meta).vec.asInstanceOf[Vec[F, V]]

      override def values(df: DataFrame[M]): F[V] =
        df.cols(index()).asInstanceOf[F[V]]

      override def get(df: DataFrame[M], row: Int): V =
        vec(df).at(values(df), row)

      override def updated(df: DataFrame[M], row: Int, value: V): DataFrame[M] =
        new DataFrame[M](
          df.meta,
          df.cols.updated(index(), vec(df).updated(values(df), row, value)))

      override def swapped(df: DataFrame[M], values: F[V]): DataFrame[M] = {
        new DataFrame[M](df.meta, df.cols.updated(index(), values))
      }
    }
}
