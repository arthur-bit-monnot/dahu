package dahu.dataframe

import dahu.dataframe.metadata.FrameMeta

trait Column[V, F[_], DF] {
  def values: F[V]
  def get(row: Int): V
  def updated(row: Int, value: V): DF
  def swapped(values: F[V]): DF
}

object Column {

  def from[K, MD <: FrameMeta](df: DataFrame[MD], k: K)(
      implicit wi: WithColumn[K, MD]): Column[wi.V, wi.F, DataFrame[MD]] =
    new Column[wi.V, wi.F, DataFrame[MD]] {
      override def values: wi.F[wi.V] = wi.values(df)

      override def get(row: Int): wi.V = wi.get(df, row)

      override def updated(row: Int, value: wi.V): DataFrame[MD] = wi.updated(df, row, value)

      override def swapped(values: wi.F[wi.V]): DataFrame[MD] = wi.swapped(df, values)
    }
}
