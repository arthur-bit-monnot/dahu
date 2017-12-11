package dahu.dataframe.utils

import shapeless._

/**
  * Typeclass witnessing a specific type is the ''i''th element of an [[HList]].
  * @author ctongfei (Tongfei Chen)
  *
  * Taken from shapeless PR #700: https://github.com/milessabin/shapeless/pull/700
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
