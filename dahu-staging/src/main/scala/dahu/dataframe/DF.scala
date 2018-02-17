package dahu.dataframe

import dahu.dataframe.errors.ColumnsOfDifferentSizes
import dahu.dataframe.metadata._
import dahu.dataframe.vector.{IndexedVector, Vec}
import shapeless.{::, HList, HNil}

case class DF[MD <: HList](cols: Vector[_])

object DF {

  def empty: DF[HNil] =
    new DF[HNil](Vector())

  implicit class DFOps[M <: HList](val df: DF[M]) extends AnyVal {

    def raw[K, CM, V, F[_]](k: K)(implicit meta: ColumnMeta.Aux[K, M, CM],
                                  value: Value.Aux[CM, V],
                                  container: Container.Aux[CM, F],
                                  index: ReverseIndexOfKey[K, M]): F[V] = {
      df.cols(index()).asInstanceOf[F[V]]
    }

    def withColumn[K0, V0, F0[_]](key: K0, values: F0[V0])(
        implicit size: RowNumber[M],
        vec: Vec[F0]): DF[ColumMetadata[K0, V0, F0] :: M] = {
      size(df) match {
        case Some(x) if x != vec.size(values) =>
          throw ColumnsOfDifferentSizes(s"New column $key has size ${vec.size(values)} != $x")
        case _ =>
      }
      type CM = ColumMetadata[K0, V0, F0]
      val colMeta: CM = new ColumMetadata[K0, V0, F0] {}
      new DF[CM :: M](df.cols :+ values)
    }

    def withContainer[A <: ColumnContainer](container: A): DF[A :: M] =
      DF[A :: M](df.cols :+ container)

    /** Base method to retrieve a column. The container type of the column is not required,
      * which restricts the operations that can made on it.
      * Look at {{{column()}}} for less restricted implementation. */
    def apply[K, V](k: K)(implicit wi: WithColumn[K, V, DF[M]]): Column[V, DF[M]] =
      Column.from(df, k)

    /** Way to retrieve a more feature full implementation of a column but
      * that requires the container type. */
    def column[K, V, F[_]](k: K)(
        implicit wi: WithColumn.Aux[K, V, F, DF[M]]): ColumnF[V, F, DF[M]] =
      ColumnF.from(df, k)

    def indexOf[K](implicit ev: ReverseIndexOfKey[K, M]): Int = ev()

    def indexed[K, V0, PrevCM, MOut <: HList](k: K)(
        implicit prev: ColumnMeta.Aux[K, M, PrevCM],
        value: Value.Aux[PrevCM, V0],
        container: Container.Aux[PrevCM, Vector],
        swapped: Swapped.Aux[K, ColumMetadata[K, V0, IndexedVector], M, MOut],
        index: ReverseIndexOfKey[K, M]): DF[MOut] = {
      val v: Vector[V0]     = raw(k)
      val map: Map[V0, Int] = v.zipWithIndex.toMap
      val col               = IndexedVector[V0](v, map)
      type CM = ColumMetadata[K, V0, IndexedVector]
      val meta: CM = new ColumMetadata[K, V0, IndexedVector] {}
      swapped(df, meta, col)
    }
  }
}
