package dahu

import algebra.Order

import scala.reflect.ClassTag

package object utils {

  private[dahu] type sp = scala.specialized
  private[dahu] type ClassTag[X] = scala.reflect.ClassTag[X]

  def allCombinations[T](inputSeq: Seq[Set[T]]): Set[List[T]] = {
    inputSeq.toList match {
      case Nil           => Set(Nil)
      case values :: Nil => values.map(List(_))
      case values :: tail =>
        allCombinations(tail).flatMap(tailComb => values.map(v => v :: tailComb))
    }
  }

  /** Alias to useful instances. */
  private val intClassTag: ClassTag[Int] = implicitly[ClassTag[Int]]
  private val intAlgebra: Order[Int] = spire.implicits.IntAlgebra

  trait IntSubset { self: Int =>
  }
  type SubInt = Int with IntSubset
  type SInt[T] = SubInt with T

  /** Simply wraps an existing class to make sure we do not mix classes with subint. */
  trait Wrapped[T] { self: SubInt =>
  }
  type SubSubInt[X <: SubInt, AdditionalTag] = X with Wrapped[AdditionalTag]

  implicit def classTagIS[T <: SubInt]: ClassTag[T] = tagged(intClassTag)
  implicit def orderingIS[T <: SubInt]: Ordering[T] = tagged(implicitly[Ordering[Int]])
  implicit def orderIS[T <: SubInt]: Order[T] = tagged(intAlgebra)

  def untagged[T <: SubInt, F[_]](v: F[T]): F[Int] = v.asInstanceOf[F[Int]]
  private def tagged[T <: SubInt, F[_]](v: F[Int]): F[T] = v.asInstanceOf[F[T]]

  implicit final class SFunctorOps[F[_], A](private val lhs: F[A]) extends AnyVal {
    def smap[@sp(Int) B: ClassTag](f: A => B)(implicit F: SFunctor[F]): F[B] =
      F.smap(lhs)(f)

    def flatMap[B](f: A => F[B])(implicit M: SMonad[F]): F[B] = M.flatMap(lhs)(f)
  }

  implicit final class SequenceOps[F[_], G[_], A](private val lhs: F[G[A]]) extends AnyVal {
    def sequence(implicit T: STraverse[F], App: SApplicative[G], ct: ClassTag[A]): G[F[A]] =
      T.sequence[G, A](lhs)

  }

}
