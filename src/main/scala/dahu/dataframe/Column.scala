package dahu.dataframe

import shapeless.Nat._0
import shapeless.ops.hlist.{Length, ToTraversable}
import shapeless.ops.nat.ToInt
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

/**
  * H is an Hlist where each elements is a subtype of Column
  * @tparam H
  */
trait MetaData[H <: HList] {
  type Fields <: HList

  val width: Int
  //  def toList[Lub](l: H)(implicit toList: ToTraversable[H,List]): toList.Out = toList(l)
  def toList(l: Fields): List[Any]
  //  def toArray(l: H): List[Any]
  //  def natSize(implicit len: Length[H]): len.Out = len()
  //  def size[N](implicit n: ToInt[Size]): Int = n()

}

object MetaData {
  type Aux[H <: HList, Fields0 <: HList] = MetaData[H] { type Fields = Fields0 }

  def apply[H <: HList](implicit ev: MetaData[H]): MetaData[H] = ev

  implicit def meta[L <: HList, LF <: HList, S <: Nat](
      implicit natLen: Length.Aux[L, S],
      len: ToInt[S],
      evToList: ToTraversable[LF, List]): MetaData.Aux[L, LF] =
    new MetaData[L] {
      override type Fields = LF
      override val width: Int = len()

      override def toList(l: Fields): List[Any] = evToList(l)
    }
}

case class DataFrame[Keys <: HList, Fields <: HList](cols: Vector[Vector[_]])(
    implicit metaEv: MetaData.Aux[Keys, Fields]) {
  private[dataframe] val meta = metaEv

  def indexOf[V, I <: Nat](witness: V)(implicit ev: IndexOf.Aux[V, Keys, I],
                                       i: ToInt[I]): Int = i()

//  def column[V, I <: Nat](witness: V)(implicit ev: IndexOf.Aux[V, Keys, I],
//                                      i: ToInt[I]): Vector[V] =
//    cols(i()).asInstanceOf[Vector[V]]

  def append(values: Fields): DataFrame[Keys, Fields] = {
    println(values)
    val valuesArray = meta.toList(values)
    println(valuesArray)
    assert(cols.size == valuesArray.size)
    val newCols =
      cols.zip(valuesArray).map { case (vec, value) => vec :+ value }
    new DataFrame[Keys, Fields](newCols)
  }

  def updated[K, V, I <: Nat](col: K, row: Int, v: V)(
      implicit ev: IndexOf.Aux[K, Keys, I],
      ev2: IndexOf.Aux[V, Fields, I],
      c: ToInt[I]): DataFrame[Keys, Fields] =
    new DataFrame(cols.updated(c(), cols(c()).updated(row, v)))

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

    def apply[K, I <: Nat](k: K)(implicit ev: DFIndexOf[K,Ks,Vs]): Col[ev.V, DataFrame[Ks, Vs]] =
      Col.extractColumn(df, k)(ev)
  }
}

trait Col[V, DF] {
  def values: Vector[V]
  def get(row: Int): V
  def updated(row: Int, value: V): DF
  def swapped(values: Vector[V]): DF
}

object Col {

  def extractColumn[K, Ks <: HList, Vs <: HList](
      df: DataFrame[Ks, Vs],
      k: K)(implicit ev: DFIndexOf[K, Ks, Vs]): Col[ev.V, DataFrame[Ks, Vs]] =
    new Col[ev.V, DataFrame[Ks, Vs]] {
      override def values: Vector[ev.V] = df.cols(ev()).asInstanceOf[Vector[ev.V]]

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

/**
  * Typeclass witnessing a specific type is the ''i''th element of an [[HList]].
  * @author ctongfei (Tongfei Chen)
  */
trait IndexOf[X, L <: HList] extends /* DepFn1[L] with */ Serializable {
  type Out <: Nat
}

object IndexOf {
  import shapeless.ops.nat._

  def apply[X, L <: HList](
      implicit indexOf: IndexOf[X, L]): Aux[X, L, indexOf.Out] = indexOf

  type Aux[X, L <: HList, Out0] = IndexOf[X, L] { type Out = Out0 }

  implicit def indexOfHead[X, T <: HList]: Aux[X, X :: T, _0] =
    new IndexOf[X, X :: T] {
      type Out = _0
      def apply(t: X :: T) = Nat._0
    }

  implicit def indexOfOthers[X, H, T <: HList, I <: Nat](
      implicit indexOf: IndexOf.Aux[X, T, I]): Aux[X, H :: T, Succ[I]] =
    new IndexOf[X, H :: T] {
      type Out = Succ[I]
    }
}

trait DFIndexOf[K, Ks <: HList, Vs <: HList] {
  type Index <: Nat
  type V

  def apply(): Int
}
object DFIndexOf {
  type Aux[K, Ks <: HList, Vs <: HList, Index0 <: Nat, V0] =
    DFIndexOf[K, Ks, Vs] { type Index = Index0; type V = V0 }

  implicit def dfIndexOfHead[K, Ks <: HList, Vs <: HList, I <: Nat, Value](
      implicit keysIndex: IndexOf.Aux[K, Ks, I],
      valuesIndex: IndexOf.Aux[Value, Vs, I], toInt: ToInt[I]) : DFIndexOf.Aux[K, Ks, Vs, I, Value] = new DFIndexOf[K, Ks, Vs] {
    override type Index = I
    override type V = Value

    override def apply(): Int = toInt()
  }
}
