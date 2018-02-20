package dahu

import algebra.Order

import scala.reflect.ClassTag

package object arrows {

  /** Alias to useful instances. */
  private val intClassTag: ClassTag[Int] = implicitly[ClassTag[Int]]
  private val intAlgebra: Order[Int]     = spire.implicits.IntAlgebra

  // TODO: remove
//  /** To specify a subset of integers in the type system.
//    *
//    * TaggedInt is constrained to be an instance of Int so that the compiler can properly infer that
//    * the actual type is Int and avoid boxing in arrays and function parameters.
//    * This is due to the fact that Int is a final class.
//    * */
//  trait TaggedInt[Tag] { self: Int =>
//  }
//  type KI[T] = Int with TaggedInt[T]
//
//  implicit def classTag[T]: ClassTag[KI[T]] = intClassTag.asInstanceOf[ClassTag[KI[T]]]
//  implicit def ordering[T]: Ordering[KI[T]] =
//    implicitly[Ordering[Int]].asInstanceOf[Ordering[KI[T]]]
//
//  implicit def order[T]: Order[KI[T]] = intAlgebra.asInstanceOf[Order[KI[T]]]

  trait IntSubset { self: Int =>
  }
  type SubInt  = Int with IntSubset
  type SInt[T] = SubInt with T

  type SubSubInt[X <: SubInt, AdditionalTag] = X with AdditionalTag

  implicit def classTagIS[T <: SubInt]: ClassTag[T] = tagged(intClassTag)
  implicit def orderingIS[T <: SubInt]: Ordering[T] = tagged(implicitly[Ordering[Int]])
  implicit def orderIS[T <: SubInt]: Order[T]       = tagged(intAlgebra)

  def untagged[T <: SubInt, F[_]](v: F[T]): F[Int]       = v.asInstanceOf[F[Int]]
  private def tagged[T <: SubInt, F[_]](v: F[Int]): F[T] = v.asInstanceOf[F[T]]
}
