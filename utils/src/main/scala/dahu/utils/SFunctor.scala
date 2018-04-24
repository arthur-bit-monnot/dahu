package dahu.utils

import scala.reflect.ClassTag
import scala.{specialized => sp}

trait SFunctor[F[_]] {

  def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: F[A])(f: A => B): F[B]

}

object SFunctor {

  def apply[F[_]](implicit instance: SFunctor[F]): SFunctor[F] = instance

  private final class FromCats[F[_]](F: cats.Functor[F]) extends SFunctor[F] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
  }

  implicit def fromCats[F[_]: cats.Functor]: SFunctor[F] =
    new FromCats[F](implicitly[cats.Functor[F]])

  implicit final class SFunctorOps[F[_], A](private val lhs: F[A]) extends AnyVal {
    def smap[@sp(Int) B: ClassTag](f: A => B)(implicit F: SFunctor[F]): F[B] = F.smap(lhs)(f)
  }
}
