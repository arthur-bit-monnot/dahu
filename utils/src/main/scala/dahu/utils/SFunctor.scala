package dahu.utils

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

}
