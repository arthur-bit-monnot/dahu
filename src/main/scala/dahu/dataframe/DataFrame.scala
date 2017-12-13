package dahu.dataframe

import dahu.dataframe.errors.ColumnsOfDifferentSizes
import dahu.dataframe.metadata._
import dahu.dataframe.vector.{IndexedVector, Vec}
import shapeless.{::, HList, HNil}

case class DataFrame[MD <: HList](meta: MD, cols: Vector[_]) {

//
//  def columnMetadata[K, KMeta <: ColMeta[K]](k: K)(
//      implicit columnMeta: ColumnMeta.Aux[K, MD, KMeta]): KMeta =
//    columnMeta.apply(meta)

}

object DataFrame {

  def empty: DataFrame[HNil] =
    new DataFrame[HNil](HNil, Vector())

  implicit class DFOps[M <: HList](val df: DataFrame[M]) extends AnyVal {

    def raw[K, CM, V, F[_]](k: K)(implicit meta: ColumnMeta.Aux[K, M, CM],
                                  value: Value.Aux[CM, V],
                                  container: Container.Aux[CM, F],
                                  index: ReverseIndexOfKey[K, M]): F[V] = {
      df.cols(index()).asInstanceOf[F[V]]
    }

    def withColumn[K0, V0, F0[_]](key: K0, values: F0[V0])(
        implicit size: RowNumber[M],
        vec: Vec[F0, V0]): DataFrame[ColumMetadata[K0, V0, F0] :: M] = {
      size(df) match {
        case Some(x) if x != vec.size(values) =>
          throw ColumnsOfDifferentSizes(s"New column $key has size ${vec.size(values)} != $x")
        case _ =>
      }
      type CM = ColumMetadata[K0, V0, F0]
      val colMeta: CM = new ColumMetadata[K0, V0, F0] {}
      new DataFrame[CM :: M](colMeta :: df.meta, df.cols :+ values)
    }

    def apply[K](k: K)(implicit wi: WithColumn[K, M]): Column[wi.V, wi.F, M] =
      Column.from(df, k)

    def indexOf[K](implicit ev: ReverseIndexOfKey[K, M]): Int = ev()

    def indexed[K, V0, PrevCM, MOut <: HList](k: K)(
        implicit prev: ColumnMeta.Aux[K, M, PrevCM],
        value: Value.Aux[PrevCM, V0],
        container: Container.Aux[PrevCM, Vector],
        swapped: Swapped.Aux[K, ColumMetadata[K, V0, IndexedVector], M, MOut],
        index: ReverseIndexOfKey[K, M]): DataFrame[MOut] = {
      val v: Vector[V0]     = raw(k)
      val map: Map[V0, Int] = v.zipWithIndex.toMap
      val col               = IndexedVector[V0](v, map)
      type CM = ColumMetadata[K, V0, IndexedVector]
      val meta: CM = new ColumMetadata[K, V0, IndexedVector] {}
      swapped(df, meta, col)
    }
  }
}
