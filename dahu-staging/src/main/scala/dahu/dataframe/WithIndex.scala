package dahu.dataframe

import dahu.dataframe.metadata._
import dahu.dataframe.vector.Index
import shapeless.HList

trait WithIndex[K, V, MD <: HList] {

  def contains(df: DF[MD], value: V): Boolean
  def id(df: DF[MD], value: V): Option[Int]

  def idUnsafe(df: DF[MD], value: V): Int

}

object WithIndex {

  implicit def fromDataFrame[K, V, F[_], MD <: HList, CM](implicit
                                                          key: Key.Aux[CM, K],
                                                          value: Value.Aux[CM, V],
                                                          container: Container.Aux[CM, F],
                                                          columnIndex: ReverseIndexOfKey[K, MD],
                                                          index: Index[F, V]): WithIndex[K, V, MD] =
    new WithIndex[K, V, MD] {

      private def content(df: DF[MD]): F[V] =
        df.cols(columnIndex()).asInstanceOf[F[V]]

      override def id(df: DF[MD], value: V): Option[Int] =
        index.id(content(df), value)

      override def contains(df: DF[MD], value: V): Boolean =
        index.contains(content(df), value)

      override def idUnsafe(df: DF[MD], value: V): Int =
        index.idUnsafe(content(df), value)
    }

}
