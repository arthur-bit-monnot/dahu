package dahu.dataframe.utils

import shapeless._
import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt

import scala.annotation.{implicitAmbiguous, implicitNotFound}

/**
  * Typeclass witnessing a specific type is the ''i''th element of an [[HList]], starting to count from the end of the List.
  *
  * For instance:
  *   ReverseIndexOf[X, Y::X::HNil] yields 0
  *   ReverseIndexOf[Y, Y::X::HNil] yields 1
  *
  *   ReverseIndexOf[X, Y::HNil] should fail due to not found implicit
  *   ReverseIndexOf[X, X::X::HNil should fail due to ambiguous implicits
  */
@implicitNotFound(msg = "No type ${X} in ${L}")
trait ReverseIndexOf[X, L <: HList] {
  type Out <: Nat
  def apply(): Int
}

trait LowPriority {

  /** Matches when the object is the head of the list.
    *
    * Type is more specific than other rule, so the priority is decreased
    * by putting it in this trait so that ambiguity arises when the object
    * is in the head AND the tail.*/
  implicit def indexOfHead[X, T <: HList, N <: Nat](
      implicit ev: Length.Aux[T, N],
      toInt: ToInt[N]): ReverseIndexOf.Aux[X, X :: T, N] =
    new ReverseIndexOf[X, X :: T] {
      override type Out = N
      override def apply(): Int = toInt()
    }
}

object ReverseIndexOf extends LowPriority {
  type Aux[X, L <: HList, Out0] = ReverseIndexOf[X, L] { type Out = Out0 }

  def apply[X, L <: HList](implicit indexOf: ReverseIndexOf[X, L]): Aux[X, L, indexOf.Out] = indexOf

  @implicitAmbiguous("Ambiguous implicit: Type ${X} appears multiple times in HList ${H} :: ${T}")
  implicit def indexOfOthers[X, H, T <: HList, N <: Nat](
      implicit ev: Aux[X, T, N]): Aux[X, H :: T, N] =
    ev.asInstanceOf[Aux[X, H :: T, N]]

}
