package dahu.dataframe

import dahu.dataframe.metadata.{ColMeta, ColumnMeta, FrameMeta, IndexOf}
import shapeless._

//trait Column[T] {
//  type Field
//}
//object Column {
//  type Aux[T, F0] = Column[T] { type Field = F0 }
//
//  def apply[T](implicit ev: Column[T]) = ev
//}
//
//trait TypedColumns[L <: HList] {
//  type Fields <: HList
//}
//object TypedColumns {
//  type Aux[L <: HList, Fields0 <: HList] = TypedColumns[L] {
//    type Fields = Fields0
//  }
//
//  def apply[L <: HList](implicit ev: TypedColumns[L]) = ev
//
//  implicit def hnil[T <: HNil]: TypedColumns.Aux[T, HNil] =
//    new TypedColumns[T] {
//      override type Fields = HNil
//    }
//
//  implicit def hlist[H, HF, T <: HList, TF <: HList](
//      implicit ev: Column.Aux[H, HF],
//      typedColumns: TypedColumns.Aux[T, TF])
//    : TypedColumns.Aux[H :: T, HF :: TF] =
//    new TypedColumns[H :: T] {
//      override type Fields = HF :: TF
//    }
//}

trait Col[V, DF] {
  def values: Vector[V]
  def get(row: Int): V
  def updated(row: Int, value: V): DF
  def swapped(values: Vector[V]): DF
}

object Col {

  def extractColumn[K, V, M <: FrameMeta, CM <: ColMeta](df: DataFrame[M], k: K)(
      implicit index: IndexOf[K, M], fieldType: ColumnMeta.Aux[K, M, CM], ev: CM <:< ColMeta.KVAux[K,V]): Col[V, DataFrame[M]] =
    new Col[V, DataFrame[M]] {
      override def values: Vector[V] =
        df.cols(index()).asInstanceOf[Vector[V]]

      override def get(row: Int): V = values(row)
      override def updated(row: Int, value: V): DataFrame[M] =
        new DataFrame[M](df.meta,
          df.cols.updated(index(), df.cols(index()).updated(row, value)))

      override def swapped(values: Vector[V]): DataFrame[M] = {
        require(df.cols(index()).size == values.size, "Error: column sizes differ")
        new DataFrame[M](df.meta, df.cols.updated(index(), values))
      }
    }

}
