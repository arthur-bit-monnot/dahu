package dahu

import algebra.Order
import cats.~>

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

  implicit def classTag[I <: Int]: ClassTag[I] = ClassTag.Int.asInstanceOf[ClassTag[I]]

//  implicit def classTagIS[T <: SubInt]: ClassTag[T] = tagged(intClassTag)
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
    def ssequence(implicit T: STraverse[F], App: SApplicative[G], ct: ClassTag[A]): G[F[A]] =
      T.sequence[G, A](lhs)

  }

  implicit final class IterableOps[A](private val lhs: Iterable[A]) extends AnyVal {
    def toVec(implicit ct: ClassTag[A]): Vec[A] = Vec.fromIterable(lhs)
  }

  /****** FunctionK manipulation ******/
  /** An existential type.
    * Essentially, Something can be freely substituted in Higher-Kinded types since it can never by inspected. */
  type Something = Any // todo: make Something opaque

  /** lifts an existentially typed Function1 to a FunctionK */
  def lift[F[_], G[_]](f: F[Something] => G[Something]): F ~> G = new (F ~> G) {
    override def apply[A](fa: F[A]): G[A] = f.asInstanceOf[F[A] => G[A]](fa)
  }

}
