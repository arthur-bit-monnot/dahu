package dahu.dataframe

import dahu.dataframe.metadata._
import shapeless._

case class DataFrame[MD <: FrameMeta](meta: MD, cols: Vector[Vector[_]]) {
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

  def withColumn[K, V](key: K, values: Vector[V])(implicit ev: ColMeta.KVAux[K, V])
    : DataFrame[ev.type ::: MD] = {
    require(cols.headOption.forall(_.size == values.size))
    new DataFrame[ev.type ::: MD]((ev ::: meta).asInstanceOf[ev.type ::: MD], cols :+ values)
  }

  def indexOf[K](implicit ev: IndexOf[K, MD]): Int = ev()
}

object DataFrame {

  def empty: DataFrame[EmptyFrame] = new DataFrame[EmptyFrame](EmptyFrame, Vector())

  implicit class DFOps[M <: FrameMeta](val df: DataFrame[M])
      extends AnyVal {

//    def apply[K, I <: Nat](k: K)(
//        implicit ev: DFIndexOf[K, Ks, Vs]): Col[ev.V, DataFrame[Ks, Vs]] =
//      Col.extractColumn(df, k)(ev)
  }
}
