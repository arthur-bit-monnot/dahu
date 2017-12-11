package dahu.dataframe

import shapeless._

case class DataFrame[Keys <: HList, Fields <: HList](cols: Vector[Vector[_]])(
    implicit metaEv: MetaData.Aux[Keys, Fields]) {
  private[dataframe] val meta = metaEv

  def append(values: Fields): DataFrame[Keys, Fields] = {
    println(values)
    val valuesArray = meta.toList(values)
    println(valuesArray)
    assert(cols.size == valuesArray.size)
    val newCols =
      cols.zip(valuesArray).map { case (vec, value) => vec :+ value }
    new DataFrame[Keys, Fields](newCols)
  }

  def withColumn[K, V](key: K, values: Vector[V])(
      implicit ev: MetaData.Aux[K :: Keys, V :: Fields])
    : DataFrame[K :: Keys, V :: Fields] = {
    require(cols.headOption.forall(_.size == values.size))
    new DataFrame[K :: Keys, V :: Fields](values +: cols)
  }
}

object DataFrame {

  def apply[A, B, Fields <: HList](a: A, b: B)(
      implicit typedCols: TypedColumns.Aux[A :: B :: HNil, Fields],
      ev: MetaData.Aux[A :: B :: HNil, Fields])
    : DataFrame[A :: B :: HNil, Fields] =
    new DataFrame(Vector.fill(2)(Vector[Any]()))(ev)

  implicit class DFOps[Ks <: HList, Vs <: HList](val df: DataFrame[Ks, Vs])
      extends AnyVal {

    def apply[K, I <: Nat](k: K)(
        implicit ev: DFIndexOf[K, Ks, Vs]): Col[ev.V, DataFrame[Ks, Vs]] =
      Col.extractColumn(df, k)(ev)
  }
}
