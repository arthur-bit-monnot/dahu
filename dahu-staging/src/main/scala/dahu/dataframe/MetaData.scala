package dahu.dataframe

import shapeless._
import shapeless.ops.hlist.{Length, ToTraversable}
import shapeless.ops.nat.ToInt

trait MetaData[H <: HList] {
  type Fields <: HList

  val width: Int
  def toList(l: Fields): List[Any]
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
