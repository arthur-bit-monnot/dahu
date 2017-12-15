package dahu.dataframe

import dahu.dataframe.metadata._
import dahu.dataframe.vector.Vec
import shapeless.HList

trait WithColumn[K, V, D] {
  type F[_]

  protected val vecInstance: Vec[F, V]

  def columnContent(df: D): F[V]

  def size(df: D): Int = vecInstance.size(columnContent(df))

  def valueAt(df: D, row: Int): V = vecInstance.at(columnContent(df), row)

  def values(df: D): Iterable[V] = vecInstance.values(columnContent(df))

  def updated(df: D, row: Int, value: V): D

  /** Replaces the content of the column by the one provided. */
  def swapped(df: D, values: F[V]): D
}

object WithColumn {
  type Aux[K, V, F0[_], D] = WithColumn[K, V, D] { type F[T] = F0[T] }

  def extractColumn[K, V, F0[_], D](k: K, extract: D => F0[V], update: (D, F0[V]) => D)(
      implicit vec: Vec[F0, V]): WithColumn.Aux[K, V, F0, D] =
    new WithColumn[K, V, D] {
      override type F[x] = F0[x]
      override protected val vecInstance: Vec[F, V] = vec

      override def columnContent(d: D): F[V] = extract(d)

      override def updated(d: D, row: Int, value: V): D =
        update(d, vecInstance.updated(extract(d), row, value))

      override def swapped(d: D, values: F[V]): D =
        update(d, values)
    }

  implicit def withColumn[K, V, F0[_], CM, M <: HList](
      implicit index: ReverseIndexOfKey[K, M],
      meta: ColumnMeta.Aux[K, M, CM],
      value: Value.Aux[CM, V],
      container: Container.Aux[CM, F0],
      vec: Vec[F0, V]
  ): WithColumn.Aux[K, V, F0, DF[M]] = new WithColumn[K, V, DF[M]] {
    override type F[x] = F0[x]
    type D             = DF[M]
    private val indexInDataFrame: Int              = index()
    override protected val vecInstance: Vec[F0, V] = vec

    override def columnContent(df: DF[M]): F[V] =
      df.cols(indexInDataFrame).asInstanceOf[F[V]]

    def updated(df: DF[M], row: Int, value: V): DF[M] =
      new DF[M](
        df.cols.updated(indexInDataFrame, vecInstance.updated(columnContent(df), row, value)))

    /** Replaces the content of the column by the one provided. */
    def swapped(df: DF[M], values: F[V]): DF[M] =
      new DF[M](df.cols.updated(indexInDataFrame, values))
  }

}
