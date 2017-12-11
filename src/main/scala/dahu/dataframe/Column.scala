package dahu.dataframe

import shapeless._

trait Column[T] {
  type Field
}
object Column {
  type Aux[T, F0] = Column[T] { type Field = F0 }

  def apply[T](implicit ev: Column[T]) = ev
}

trait TypedColumns[L <: HList] {
  type Fields <: HList
}
object TypedColumns {
  type Aux[L <: HList, Fields0 <: HList] = TypedColumns[L] {
    type Fields = Fields0
  }

  def apply[L <: HList](implicit ev: TypedColumns[L]) = ev

  implicit def hnil[T <: HNil]: TypedColumns.Aux[T, HNil] =
    new TypedColumns[T] {
      override type Fields = HNil
    }

  implicit def hlist[H, HF, T <: HList, TF <: HList](
      implicit ev: Column.Aux[H, HF],
      typedColumns: TypedColumns.Aux[T, TF])
    : TypedColumns.Aux[H :: T, HF :: TF] =
    new TypedColumns[H :: T] {
      override type Fields = HF :: TF
    }
}

trait Col[V, DF] {
  def values: Vector[V]
  def get(row: Int): V
  def updated(row: Int, value: V): DF
  def swapped(values: Vector[V]): DF
}

object Col {

  def extractColumn[K, Ks <: HList, Vs <: HList](df: DataFrame[Ks, Vs], k: K)(
      implicit ev: DFIndexOf[K, Ks, Vs]): Col[ev.V, DataFrame[Ks, Vs]] =
    new Col[ev.V, DataFrame[Ks, Vs]] {
      override def values: Vector[ev.V] =
        df.cols(ev()).asInstanceOf[Vector[ev.V]]

      override def get(row: Int): ev.V = values(row)
      override def updated(row: Int, value: ev.V): DataFrame[Ks, Vs] =
        new DataFrame[Ks, Vs](
          df.cols.updated(ev(), df.cols(ev()).updated(row, value)))(df.meta)

      override def swapped(values: Vector[ev.V]): DataFrame[Ks, Vs] = {
        require(df.cols(ev()).size == values.size, "Error: column sizes differ")
        new DataFrame[Ks, Vs](df.cols.updated(ev(), values))(df.meta)
      }
    }

}
