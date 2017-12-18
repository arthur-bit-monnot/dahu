package dahu.dataframe

import dahu.dataframe.metadata._
import dahu.dataframe.utils.ReverseIndexOf
import dahu.dataframe.vector.Vec
import shapeless.{::, HList}

trait WithColumn[K, V, D] {
  type F[_]

  val vecInstance: Vec[F]

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

  def extractColumn[K, V, F0[_], D](extract: D => F0[V], update: (D, F0[V]) => D)(
      implicit vec: Vec[F0]): WithColumn.Aux[K, V, F0, D] =
    new WithColumn[K, V, D] {
      override type F[x] = F0[x]
      override val vecInstance: Vec[F] = vec

      override def columnContent(d: D): F[V] = extract(d)

      override def updated(d: D, row: Int, value: V): D =
        update(d, vecInstance.updated(extract(d), row, value))

      override def swapped(d: D, values: F[V]): D =
        update(d, values)
    }.asInstanceOf[WithColumn.Aux[K, V, F0, D]] // to remove red squiggles in IntelliJ

  implicit def withUnaryColumnInHead[K, V, F[_], H, T <: HList](
      implicit index: ReverseIndexOf[H, H :: T],
      key: Key.Aux[H, K],
      value: Value.Aux[H, V],
      container: Container.Aux[H, F],
      vec: Vec[F]
  ): WithColumn.Aux[K, V, F, DF[H :: T]] =
    extractColumn[K, V, F, DF[H :: T]](
      df => df.cols(index()).asInstanceOf[F[V]],
      (df, vs) => DF(df.cols.updated(index(), vs))
    )

  implicit def withNestedColumnInHead[K, V, F[_], H, T <: HList](
      implicit
      nested: WithColumn.Aux[K, V, F, H],
      index: ReverseIndexOf[H, H :: T]
  ): WithColumn.Aux[K, V, F, DF[H :: T]] = {
    extractColumn[K, V, F, DF[H :: T]](
      df => nested.columnContent(df.cols(index()).asInstanceOf[H]),
      (df, vs) => DF(df.cols.updated(index(), nested.swapped(df.cols(index()).asInstanceOf[H], vs)))
    )(nested.vecInstance)
  }

  implicit def withColumnInOthers[K, V, F[_], H, T <: HList](
      implicit
      inTail: WithColumn.Aux[K, V, F, DF[T]]
  ): WithColumn.Aux[K, V, F, DF[H :: T]] =
    inTail.asInstanceOf[Aux[K, V, F, DF[H :: T]]]
}
