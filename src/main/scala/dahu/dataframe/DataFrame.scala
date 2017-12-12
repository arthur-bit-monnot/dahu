package dahu.dataframe

import dahu.dataframe.metadata._
import dahu.dataframe.vector.Vec

case class DataFrame[MD <: FrameMeta](meta: MD, cols: Vector[_]) {
  type MetaData = MD

//  def append(values: Fields): DataFrame[Keys, Fields] = {
//    println(values)
//    val valuesArray = meta.toList(values)
//    println(valuesArray)
//    assert(cols.size == valuesArray.size)
//    val newCols =
//      cols.zip(valuesArray).map { case (vec, value) => vec :+ value }
//    new DataFrame[Keys, Fields](newCols)
//  }

  def withColumn[K0, V0, F0[_]](key: K0, values: F0[V0])(
      implicit vec0: Vec[F0, V0]): DataFrame[ColMeta.Aux[K0, V0, F0] ::: MD] = {
    type CM = ColMeta.Aux[K0, V0, F0]
    val colMeta: CM = new ColMeta {
      override type F[A] = F0[A]
      override type V = V0
      override type K = K0
      override val vec = vec0
    }
    // TODO: make the cast unnecessary here
    new DataFrame[CM ::: MD]((colMeta ::: meta).asInstanceOf[CM ::: MD],
                              cols :+ values)
  }

  def indexOf[K](implicit ev: IndexOf[K, MD]): Int = ev()

  def columnMetadata[K, KMeta <: ColMeta](k: K)(
      implicit columnMeta: ColumnMeta.Aux[K, MD, KMeta]): KMeta =
    columnMeta.apply(meta)

}

object DataFrame {

  def empty: DataFrame[EmptyFrame] =
    new DataFrame[EmptyFrame](EmptyFrame, Vector())

  implicit class DFOps[M <: FrameMeta](val df: DataFrame[M]) extends AnyVal {

    def apply[K, V, CM <: ColMeta, F[_]](k: K)(
        implicit wi: WithColumn[K, V, F, M]): Column[V, F, DataFrame[M]] =
      Column.from(df, k)

//    def apply[K, I <: Nat](k: K)(): Col[ev.V, DataFrame[Ks, Vs]] =
//      Col.extractColumn(df, k)(ev)
  }
}
