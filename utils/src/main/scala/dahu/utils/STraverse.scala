package dahu.utils

trait STraverse[F[_]] extends SFunctor[F] { self =>

  def traverse[G[_]: SApplicative, A, B: ClassTag](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: SApplicative, A: ClassTag](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: F[A])(f: A => B): F[B] =
    traverse[cats.Id, A, B](fa)(f)
}

object STraverse {

  def apply[F[_]](implicit instance: STraverse[F]): STraverse[F] = instance

  // TODO: implicit conversion from cats.

}
