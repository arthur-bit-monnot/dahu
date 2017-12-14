package dahu.dataframe

import dahu.dataframe.Column.ColumnImpl
import shapeless.HList

/**
  * Column view that is independent of the container.
  *
  * @tparam V Field type
  * @tparam MD Metadata of the dataframe containing this column.
  */
trait Column[V, MD <: HList] {
  def size: Int
  def values: Iterable[V]
  def valueAt(row: Int): V
  def updated(row: Int, value: V): DataFrame[MD]
}

object Column {

  class ColumnImpl[K, V, MD <: HList](df: DataFrame[MD], wi: WithColumn[K, V, MD])
      extends Column[V, MD] {
    override def size: Int = wi.size(df)

    override def valueAt(row: Int): V = wi.valueAt(df, row)

    override def values: Iterable[V] = wi.values(df)

    override def updated(row: Int, value: V): DataFrame[MD] = wi.updated(df, row, value)
  }

  def from[K, V, MD <: HList](df: DataFrame[MD], k: K)(
      implicit wi: WithColumn[K, V, MD]): Column[V, MD] =
    new ColumnImpl(df, wi)
}

/**
  * Column view that adds access to the raw content.
  *
  * @tparam V Type of the fields
  * @tparam F Container type.
  * @tparam MD Metadata of the dataframe
  */
trait ColumnF[V, F[_], MD <: HList] extends Column[V, MD] {
  def content: F[V]
  def swapped(values: F[V]): DataFrame[MD]
}

object ColumnF {

  class ColumnFImpl[K, V, F[_], MD <: HList](df: DataFrame[MD], wi: WithColumn.Aux[K, V, F, MD])
      extends ColumnImpl(df, wi)
      with ColumnF[V, F, MD] {
    override def content: F[V] = wi.columnContent(df)

    override def swapped(values: F[V]): DataFrame[MD] = wi.swapped(df, values)
  }

  def from[K, V, F[_], MD <: HList](df: DataFrame[MD], k: K)(
      implicit wi: WithColumn.Aux[K, V, F, MD]): ColumnF[V, F, MD] =
    new ColumnFImpl(df, wi)
}
