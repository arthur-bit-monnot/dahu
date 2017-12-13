package dahu.dataframe.utils

import shapeless._
import shapeless.ops.hlist.Length

/**
  * Typeclass witnessing a specific type is the ''i''th element of an [[HList]].
  * @author ctongfei (Tongfei Chen)
  *
  * Taken from shapeless PR #700: https://github.com/milessabin/shapeless/pull/700
  */
trait ReverseIndexOf[X, L <: HList] extends /* DepFn1[L] with */ Serializable {
  type Out <: Nat
}

object ReverseIndexOf {
  import shapeless.ops.nat._

  def apply[X, L <: HList](
      implicit indexOf: ReverseIndexOf[X, L]): Aux[X, L, indexOf.Out] = indexOf

  type Aux[X, L <: HList, Out0] = ReverseIndexOf[X, L] { type Out = Out0 }

  implicit def indexOfHead[X, T <: HList, N <: Nat](implicit ev: Length.Aux[T, N]): Aux[X, X :: T, N] = new ReverseIndexOf[X, X :: T] {
    override type Out = N
  }
  implicit def indexOfOthers[X, H, T <: HList, N <: Nat](implicit ev: Aux[X, T, N]): Aux[X, H :: T, N] = new ReverseIndexOf[X, H::T] {
    override type Out = N
  }
}
