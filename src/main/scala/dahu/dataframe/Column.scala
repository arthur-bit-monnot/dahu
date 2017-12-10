package dahu.dataframe

import shapeless.Nat._0
import shapeless.ops.hlist.{Length, ToTraversable}
import shapeless.ops.nat.ToInt
import shapeless._


trait Column {

  type FieldType


}

object StringCol {

}

trait MetaData[H <: HList] {
  type Size <: Nat

  val width: Int
//  def toList[Lub](l: H)(implicit toList: ToTraversable[H,List]): toList.Out = toList(l)
  def toList(l: H): List[Any]
//  def toArray(l: H): List[Any]
//  def natSize(implicit len: Length[H]): len.Out = len()
//  def size[N](implicit n: ToInt[Size]): Int = n()


}

object MetaData {

  def apply[H <: HList](implicit ev: MetaData[H]): MetaData[H] = ev

  implicit def meta[H <: HList, S <: Nat](implicit natLen: Length.Aux[H, S], len: ToInt[S], evToList: ToTraversable[H, List]) = new MetaData[H] {
    override type Size = S
    override val width: Int = len()

    override def toList(l: H): List[Any] = evToList(l)
  }
}


case class DataFrame[H <: HList : MetaData](cols: Vector[Vector[_]]) {
  private val meta = the[MetaData[H]]

  def indexOf[V, I <: Nat](witness: V)(implicit ev: IndexOf.Aux[V, H, I], i: ToInt[I]): Int = i()

  def column[V, I <: Nat](witness: V)(implicit ev: IndexOf.Aux[V, H, I], i: ToInt[I]): Vector[V] =
    cols(i()).asInstanceOf[Vector[V]]

  def append(values: H): DataFrame[H] = {
    println(values)
    val valuesArray = meta.toList(values)
    println(valuesArray)
    assert(cols.size == valuesArray.size)
    val newCols = cols.zip(valuesArray).map { case (vec, value) => vec :+ value }
    new DataFrame[H](newCols)
  }

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