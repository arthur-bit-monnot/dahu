package dahu.dataframe

import dahu.dataframe.metadata._
import dahu.dataframe.vector.{IndexedVector, Vec}
import shapeless.{::, HList, HNil}

case class DataFrame[MD <: HList](meta: MD, cols: Vector[_]) {

  def withColumn[K0, V0, F0[_]](key: K0,
                                values: F0[V0])(): DataFrame[ColumMetadata[K0, V0, F0] :: MD] = {
    type CM = ColumMetadata[K0, V0, F0]
    val colMeta: CM = new ColumMetadata[K0, V0, F0] {}
    new DataFrame[CM :: MD](colMeta :: meta, cols :+ values)
  }
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

    def apply[K](k: K)(implicit wi: WithColumn[K, M]): Column[wi.V, wi.F, M] =
      Column.from(df, k)

    def indexOf[K](implicit ev: ReverseIndexOfKey[K, M]): Int = ev()
//    def indexed[K, V0, PrevCM <: ColMeta[K], Out <: FrameMeta](k: K)(
//        implicit withCol: WithColumn.Aux[K, V0, Vector, M],
//        swap: Swapped.Aux[K, V0, IndexedVector, ColMeta.Aux[K, V0, IndexedVector], M, Out],
//        ev: Vec[IndexedVector, V0]
//    ): DataFrame[Out] = {
//      val v: Vector[V0] = withCol.values(df)
//      val map: Map[V0, Int] = v.zipWithIndex.toMap
//      val col = IndexedVector[V0](v, map)
//      type CM = ColMeta.Aux[K, V0, IndexedVector]
//      val meta: CM = new ColMeta[K] {
//        override type F[T] = IndexedVector[T]
//        override type V = V0
//        override val vec: Vec[IndexedVector, V0] = ev
//      }
//      swap(df, meta, col)
//    }
  }
}
