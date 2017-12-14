package dahu.dataframe

import dahu.dataframe.metadata.{Key, Value}
import shapeless.{::, HList, HNil}

/**
  * Typeclass to compute the number of rows in a DataFrame.
  *
  * @tparam MD Type encoding the metadata of a DataFrame
  */
trait RowNumber[MD <: HList] {

  /** Returns None if the dataframe has no columns. Otherwise the number of rows in the dataframe is returned in Some(_).
    * Note that each column of a given dataframe has the same number of rows. */
  def apply(df: DataFrame[MD]): Option[Int]
}

object RowNumber {

  implicit def sizeByHead[H, V, T <: HList, K](
      implicit key: Key.Aux[H, K],
      value: Value.Aux[H, V],
      withColumn: WithColumn[K, V, H :: T]): RowNumber[H :: T] =
    new RowNumber[H :: T] {
      override def apply(df: DataFrame[H :: T]): Option[Int] = Some(withColumn.size(df))
    }

  implicit def sizeOfEmpty: RowNumber[HNil] = new RowNumber[HNil] {
    override def apply(df: DataFrame[HNil]): Option[Int] = None
  }

  /** If we have an evidence that there is a column in this dataframe, reuse this one to compute the size. */
  implicit def rowNumberFromWithColumn[K, V, MD <: HList](
      implicit withColumn: WithColumn[K, V, MD]): RowNumber[MD] = new RowNumber[MD] {
    override def apply(df: DataFrame[MD]): Option[Int] = Some(withColumn.size(df))
  }
}
