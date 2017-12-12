package dahu.dataframe

import dahu.dataframe.metadata.FrameMeta

trait Column[V, F[_], DF] {
  def values: F[V]
  def get(row: Int): V
  def updated(row: Int, value: V): DF
  def swapped(values: F[V]): DF
}

object Column {

  def from[K, V, F[_], MD <: FrameMeta](df: DataFrame[MD], k: K)(
      implicit wi: WithColumn[K, V, F, MD]): Column[V, F, DataFrame[MD]] =
    new Column[V, F, DataFrame[MD]] {
      override def values: F[V] = wi.values(df)

      override def get(row: Int): V = wi.get(df, row)

      override def updated(row: Int, value: V): DataFrame[MD] = wi.updated(df, row, value)

      override def swapped(values: F[V]): DataFrame[MD] = wi.swapped(df, values)
    }
}
