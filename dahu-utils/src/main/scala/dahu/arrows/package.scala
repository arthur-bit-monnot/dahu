package dahu

import algebra.Order

import scala.reflect.ClassTag

package object arrows {

  private val intClassTag: ClassTag[Int] = implicitly[ClassTag[Int]]
  private val intAlgebra: Order[Int]     = spire.implicits.IntAlgebra

  /** To specify a subset of integers in the type system.
    *
    * TaggedInt is constrained to be an instance of Int so that the compiler can properly infer that
    * the actual type is Int and avoid boxing in arrays and function parameters.
    * This is due to the fact that Int is a final class.
    * */
  trait TaggedInt[Tag] { self: Int =>
  }
  type KI[T] = Int with TaggedInt[T]

  implicit def classTag[T]: ClassTag[KI[T]] = intClassTag.asInstanceOf[ClassTag[KI[T]]]
  implicit def ordering[T]: Ordering[KI[T]] =
    implicitly[Ordering[Int]].asInstanceOf[Ordering[KI[T]]]

  implicit def order[T]: Order[KI[T]] = intAlgebra.asInstanceOf[Order[KI[T]]]

}
