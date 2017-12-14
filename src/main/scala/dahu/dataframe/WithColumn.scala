package dahu.dataframe

import dahu.dataframe.metadata._
import dahu.dataframe.vector.Vec
import shapeless.HList

trait WithColumn[K, V, MD <: HList] {
  type F[_]

  protected val vecInstance: Vec[F, V]

  /** Index of the column in the dataframe's internal structure. */
  protected val indexInDataFrame: Int

  def columnContent(df: DataFrame[MD]): F[V] = df.cols(indexInDataFrame).asInstanceOf[F[V]]

  def size(df: DataFrame[MD]): Int = vecInstance.size(columnContent(df))

  def valueAt(df: DataFrame[MD], row: Int): V = vecInstance.at(columnContent(df), row)

  def values(df: DataFrame[MD]): Iterable[V] = vecInstance.values(columnContent(df))

  def updated(df: DataFrame[MD], row: Int, value: V): DataFrame[MD] =
    new DataFrame[MD](
      df.meta,
      df.cols.updated(indexInDataFrame, vecInstance.updated(columnContent(df), row, value)))

  /** Replaces the content of the column by the one provided. */
  def swapped(df: DataFrame[MD], values: F[V]): DataFrame[MD] =
    new DataFrame[MD](df.meta, df.cols.updated(indexInDataFrame, values))
}

object WithColumn {
  type Aux[K, V, F0[_], MD <: HList] = WithColumn[K, V, MD] { type F[T] = F0[T] }

  implicit def withColumn[K, V, F0[_], CM, M <: HList](
      implicit index: ReverseIndexOfKey[K, M],
      meta: ColumnMeta.Aux[K, M, CM],
      value: Value.Aux[CM, V],
      container: Container.Aux[CM, F0],
      vec: Vec[F0, V]
  ): WithColumn.Aux[K, V, F0, M] = new WithColumn[K, V, M] {
    override type F[x] = F0[x]

    override protected val indexInDataFrame: Int   = index()
    override protected val vecInstance: Vec[F0, V] = vec
  }

}
