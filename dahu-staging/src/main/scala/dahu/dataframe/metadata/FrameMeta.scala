package dahu.dataframe.metadata

import shapeless._

import scala.annotation.{implicitAmbiguous, implicitNotFound}
@implicitNotFound(msg = "Could not find key `${K}` in frame metadata `${M}`")
trait ColumnMeta[K, M <: HList] {
  type Out
  def apply(l: M): Out
}

object ColumnMeta extends ColumnMeta0 {
  type Aux[K, M <: HList, Out0] = ColumnMeta[K, M] { type Out = Out0 }

  def apply[K, M <: HList](implicit ev: ColumnMeta[K, M]): Aux[K, M, ev.Out] =
    ev

  @implicitAmbiguous("Multiples types with the same key ${K} in ${H :: T}")
  implicit def columnTypeOfOthers[K, H, T <: HList, Out0](
      implicit ev: ColumnMeta.Aux[K, T, Out0]): ColumnMeta.Aux[K, H :: T, Out0] = {
    new ColumnMeta[K, H :: T] {
      override type Out = Out0

      override def apply(l: H :: T): Out = ev.apply(l.tail)
    }
  }
}

trait ColumnMeta0 {

  implicit def columnTypeOfHead[K, H, T <: HList](
      implicit key: Key.Aux[H, K]): ColumnMeta.Aux[K, H :: T, H] = {
    new ColumnMeta[K, H :: T] {
      override type Out = H

      override def apply(l: H :: T): H = l.head
    }
  }
}
