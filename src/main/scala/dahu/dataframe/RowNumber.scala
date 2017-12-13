package dahu.dataframe

import dahu.dataframe.metadata.Key
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

  implicit def sizeByHead[H, T <: HList, K](implicit key: Key.Aux[H, K],
                                            withColumn: WithColumn[K, H :: T]): RowNumber[H :: T] =
    new RowNumber[H :: T] {
      override def apply(df: DataFrame[H :: T]): Option[Int] = Some(withColumn.size(df))
    }

  implicit def sizeOfEmpty: RowNumber[HNil] = new RowNumber[HNil] {
    override def apply(df: DataFrame[HNil]): Option[Int] = None
  }
}
