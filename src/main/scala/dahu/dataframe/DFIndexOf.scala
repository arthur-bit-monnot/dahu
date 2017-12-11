package dahu.dataframe

import shapeless._
import dahu.dataframe.utils.IndexOf
import shapeless.ops.nat.ToInt

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
      valuesIndex: IndexOf.Aux[Value, Vs, I],
      toInt: ToInt[I]): DFIndexOf.Aux[K, Ks, Vs, I, Value] =
    new DFIndexOf[K, Ks, Vs] {
      override type Index = I
      override type V = Value

      override def apply(): Int = toInt()
    }
}
