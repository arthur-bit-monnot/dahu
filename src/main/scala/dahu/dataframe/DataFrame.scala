package dahu.dataframe

import dahu.dataframe.metadata._
import dahu.dataframe.vector.{IndexedVector, Vec}

case class DataFrame[MD <: FrameMeta](meta: MD, cols: Vector[_]) {


//  def withColumn[K0, V0, F0[_]](key: K0, values: F0[V0])(
//      implicit vec0: Vec[F0, V0]): DataFrame[ColMeta.Aux[K0, V0, F0] ::: MD] = {
//    type CM = ColMeta.Aux[K0, V0, F0]
//    val colMeta: CM = new ColMeta[K0] {
//      override type F[A] = F0[A]
//      override type V = V0
//      override val vec = vec0
//    }
//    new DataFrame[CM ::: MD](colMeta ::: meta, cols :+ values)
//  }
//
//  def indexOf[K](implicit ev: IndexOf[K, MD]): Int = ev()
//
//  def columnMetadata[K, KMeta <: ColMeta[K]](k: K)(
//      implicit columnMeta: ColumnMeta.Aux[K, MD, KMeta]): KMeta =
//    columnMeta.apply(meta)

}

object DataFrame {

  def empty: DataFrame[EmptyFrame] =
    new DataFrame[EmptyFrame](EmptyFrame, Vector())

  implicit class DFOps[M <: FrameMeta](val df: DataFrame[M]) extends AnyVal {

    def apply[K](k: K)(
        implicit wi: WithColumn[K, M]): Column[wi.V, wi.F, M] =
      Column.from(df, k)

    def indexed[K, V0, PrevCM <: ColMeta[K], Out <: FrameMeta](k: K)(
        implicit withCol: WithColumn.Aux[K, V0, Vector, M],
        swap: Swapped.Aux[K, V0, IndexedVector, ColMeta.Aux[K, V0, IndexedVector], M, Out],
        ev: Vec[IndexedVector, V0]
    ): DataFrame[Out] = {
      val v: Vector[V0] = withCol.values(df)
      val map: Map[V0, Int] = v.zipWithIndex.toMap
      val col = IndexedVector[V0](v, map)
      type CM = ColMeta.Aux[K, V0, IndexedVector]
      val meta: CM = new ColMeta[K] {
        override type F[T] = IndexedVector[T]
        override type V = V0
        override val vec: Vec[IndexedVector, V0] = ev
      }
      swap(df, meta, col)
    }
//    def apply[K, I <: Nat](k: K)(): Col[ev.V, DataFrame[Ks, Vs]] =
//      Col.extractColumn(df, k)(ev)
  }
}
