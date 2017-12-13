package dahu.dataframe

import dahu.dataframe.metadata._
import dahu.dataframe.vector.Vec
import shapeless.HList

trait WithColumn[K, MD <: HList] {
  type V
  type F[_]

  def size(df: DataFrame[MD]): Int
  def values(df: DataFrame[MD]): F[V]
  def get(df: DataFrame[MD], row: Int): V
  def updated(df: DataFrame[MD], row: Int, value: V): DataFrame[MD]
  def swapped(df: DataFrame[MD], values: F[V]): DataFrame[MD]
}

object WithColumn {
  type Aux[K, V0, F0[_], MD <: HList] = WithColumn[K, MD] { type V = V0; type F[T] = F0[T] }

  implicit def withColumn[K, V0, F0[_], CM, M <: HList](
      implicit index: ReverseIndexOfKey[K, M],
      meta: ColumnMeta.Aux[K, M, CM],
      value: Value.Aux[CM, V0],
      container: Container.Aux[CM, F0],
      vec: Vec[F0, V0]
  ): WithColumn.Aux[K, V0, F0, M] = new WithColumn[K, M] {
    override type V    = V0
    override type F[x] = F0[x]

    override def size(df: DataFrame[M]): Int = vec.size(values(df))

    override def values(df: DataFrame[M]): F0[V0] = df.cols(index()).asInstanceOf[F[V]]

    override def get(df: DataFrame[M], row: Int): V0 = vec.at(values(df), row)

    override def updated(df: DataFrame[M], row: Int, value: V0): DataFrame[M] =
      new DataFrame[M](df.meta, df.cols.updated(index(), vec.updated(values(df), row, value)))

    override def swapped(df: DataFrame[M], values: F0[V0]): DataFrame[M] =
      new DataFrame[M](df.meta, df.cols.updated(index(), values))
  }
}
