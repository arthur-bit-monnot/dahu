package dahu.dataframe

import dahu.dataframe.metadata._

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
    type XXX = ColMeta.Aux[K0, V0, F0]
    val colMeta: XXX = new ColMeta {
      override type F[A] = F0[A]
      override type V = V0
      override type K = K0
      override val vec = vec0
    }
    new DataFrame[XXX ::: MD]((colMeta ::: meta).asInstanceOf[XXX ::: MD],
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
        implicit index: IndexOf[K, M],
        fieldType: ColumnMeta.Aux[K, M, CM],
        ev: CM <:< ColMeta.Aux[K, V, F]): Col[V, F, DataFrame[M]] =
      Col.extractColumn(df, k)

//    def apply[K, I <: Nat](k: K)(): Col[ev.V, DataFrame[Ks, Vs]] =
//      Col.extractColumn(df, k)(ev)
  }
}
