package dahu.dataframe

import dahu.dataframe.metadata.{ColMeta, ColumnMeta, FrameMeta, ReverseIndexOfKey}
import dahu.dataframe.vector.Index
import shapeless.HList

trait WithIndex[K, V, MD <: HList] {

  def id(df: DataFrame[MD], value: V): Option[Int]

}

object WithIndex {

//  implicit def fromDataFrame[K, V, F[_], MD <: FrameMeta, CM <: ColMeta[K]](
//      implicit columnIndex: IndexOf[K, MD],
//      fieldType: ColumnMeta.Aux[K, MD, CM],
//      ev: CM <:< ColMeta.Aux[K, V, F],
//      index: Index[F, V]): WithIndex[K, V, MD] =
//    new WithIndex[K, V, MD] {
//
//      private def values(df: DataFrame[MD]): F[V] = df.cols(columnIndex()).asInstanceOf[F[V]]
//
//      override def id(df: DataFrame[MD], value: V): Option[Int] = {
//        index.id(values(df), value)
//      }
//  }

}