package dahu.utils

trait SMonad[F[_]] extends SApplicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(f.andThen(pure(_)))

  override def ap[A, B: ClassTag](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff)(f => smap(fa)(f))
}

object SMonad {

  def apply[F[_]](implicit instance: SMonad[F]): SMonad[F] = instance

}
