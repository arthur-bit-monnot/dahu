package dahu.dataframe

import dahu.dataframe.Column.ColumnImpl
import shapeless.HList

/**
  * Column view that is independent of the container.
  *
  * @tparam V Field type
  * @tparam D Type of the datastructure containing the column
  */
trait Column[V, D] {
  def size: Int
  def values: Iterable[V]
  def valueAt(row: Int): V
  def updated(row: Int, value: V): D
}

object Column {

  class ColumnImpl[K, V, D](df: D, wi: WithColumn[K, V, D]) extends Column[V, D] {
    override def size: Int = wi.size(df)

    override def valueAt(row: Int): V = wi.valueAt(df, row)

    override def values: Iterable[V] = wi.values(df)

    override def updated(row: Int, value: V): D = wi.updated(df, row, value)
  }

  def from[K, V, D](df: D, k: K)(implicit wi: WithColumn[K, V, D]): Column[V, D] =
    new ColumnImpl(df, wi)
}

/**
  * Column view that adds access to the raw content.
  *
  * @tparam V Type of the fields
  * @tparam F Container type.
  * @tparam D Type of the data structure containing the column
  */
trait ColumnF[V, F[_], D] extends Column[V, D] {
  def content: F[V]
  def swapped(values: F[V]): D
}

object ColumnF {

  class ColumnFImpl[K, V, F[_], D](df: D, wi: WithColumn.Aux[K, V, F, D])
      extends ColumnImpl(df, wi)
      with ColumnF[V, F, D] {
    override def content: F[V] = wi.columnContent(df)

    override def swapped(values: F[V]): D = wi.swapped(df, values)
  }

  def from[K, V, F[_], D](df: D, k: K)(implicit wi: WithColumn.Aux[K, V, F, D]): ColumnF[V, F, D] =
    new ColumnFImpl(df, wi)
}

trait IndexedColumn[V, MD <: HList] {
  def id(v: V): Option[Int]
  def contains(v: V): Boolean
  def idUnsafe(v: V): Int
}
object IndexedColumn {

  def from[K, V, MD <: HList](df: DF[MD], k: K)(
      implicit wi: WithIndex[K, V, MD]): IndexedColumn[V, MD] =
    new IndexedColumn[V, MD] {
      override def id(v: V): Option[Int]   = wi.id(df, v)
      override def contains(v: V): Boolean = wi.contains(df, v)
      override def idUnsafe(v: V): Int     = wi.idUnsafe(df, v)
    }
}
