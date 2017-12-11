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
  type Aux[L <: HList,Fields0 <: HList] = TypedColumns[L] { type Fields = Fields0 }

  def apply[L <: HList](implicit ev: TypedColumns[L]) = ev

  implicit def hnil[T <: HNil]: TypedColumns.Aux[T, HNil] = new TypedColumns[T] {
    override type Fields = HNil
  }

  implicit def hlist[H, HF, T <: HList, TF <: HList](implicit ev: Column.Aux[H, HF], typedColumns: TypedColumns.Aux[T, TF]) : TypedColumns.Aux[H::T, HF::TF] =
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
                                                        fields: TypedColumns.Aux[L, LF],
                                                        len: ToInt[S],
                                                        evToList: ToTraversable[LF, List]) = new MetaData[L] {
    override type Fields = LF
    override val width: Int = len()

    override def toList(l: Fields): List[Any] = evToList(l)
  }
}


case class DataFrame[H <: HList, Fields <: HList](cols: Vector[Vector[_]])(implicit meta: MetaData.Aux[H, Fields]) {


  def indexOf[V, I <: Nat](witness: V)(implicit ev: IndexOf.Aux[V, H, I], i: ToInt[I]): Int = i()

  def column[V, I <: Nat](witness: V)(implicit ev: IndexOf.Aux[V, H, I], i: ToInt[I]): Vector[V] =
    cols(i()).asInstanceOf[Vector[V]]

  def append(values: Fields): DataFrame[H, Fields] = {
    println(values)
    val valuesArray = meta.toList(values)
    println(valuesArray)
    assert(cols.size == valuesArray.size)
    val newCols = cols.zip(valuesArray).map { case (vec, value) => vec :+ value }
    new DataFrame[H, Fields](newCols)
  }

  def updated[K,V,I <:Nat](col: K, row: Int, v: V)(implicit ev: IndexOf.Aux[K, H, I],ev2: IndexOf.Aux[V, Fields, I], c: ToInt[I]): DataFrame[H,Fields] =
    new DataFrame(cols.updated(c(), cols(c()).updated(row, v)))

}

object DataFrame {

  def apply[A : Column, B: Column](a: A, b: B)(implicit ev: MetaData[A::B::HNil]):
  DataFrame[A::B::HNil, ev.Fields]
  = new DataFrame(Vector.fill(2)(Vector[Any]()))(ev)
}

  /**
   * Typeclass witnessing a specific type is the ''i''th element of an [[HList]].
   * @author ctongfei (Tongfei Chen)
   */
  trait IndexOf[X, L <: HList] extends /* DepFn1[L] with */ Serializable { type Out <: Nat }

  object IndexOf {
    import shapeless.ops.nat._

    def apply[X, L <: HList](implicit indexOf: IndexOf[X, L]): Aux[X, L, indexOf.Out] = indexOf

    type Aux[X, L <: HList, Out0] = IndexOf[X, L] { type Out = Out0 }

    implicit def indexOfHead[X, T <: HList]: Aux[X, X :: T, _0] =
      new IndexOf[X, X :: T] {
        type Out = _0
        def apply(t: X :: T) = Nat._0
      }

    implicit def indexOfOthers[X, H, T <: HList, I <: Nat, J <: Nat]
    (implicit indexOf: IndexOf.Aux[X, T, I]): Aux[X, H :: T, Succ[I]] =
      new IndexOf[X, H :: T] {
        type Out = Succ[I]
      }
}