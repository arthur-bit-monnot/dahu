package dahu.utils

trait SApplicative[F[_]] extends SFunctor[F] { self =>

  def pure[A: ClassTag](x: A): F[A]

  def ap[A, B: ClassTag](ff: F[A => B])(fa: F[A]): F[B]

  override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: F[A])(f: A => B): F[B] =
    ap(pure(f))(fa)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(smap(fa)(a => (b: B) => (a, b)))(fb)

  def map2[A, B, Z: ClassTag](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    smap(product(fa, fb))(f.tupled)

  def compose[G[_]: SApplicative: ClassTagK]: SApplicative[λ[α => F[G[α]]]] =
    new SApplicative.ComposedApplicative[F, G](self, SApplicative[G])
}

object SApplicative {

  def apply[F[_]](implicit instance: SApplicative[F]): SApplicative[F] = instance

  private final class FromCats[F[_]](F: cats.Applicative[F]) extends SApplicative[F] {
    override def pure[A: ClassTag](x: A): F[A] = F.pure(x)
    override def ap[A, B: ClassTag](ff: F[A => B])(fa: F[A]): F[B] = F.ap(ff)(fa)
  }

  implicit def fromCats[F[_]: cats.Applicative]: SApplicative[F] =
    new FromCats[F](implicitly[cats.Applicative[F]])

  private[utils] class ComposedApplicative[F[_], G[_]](
      val F: SApplicative[F],
      val G: SApplicative[G])(implicit ctgk: ClassTagK[G])
      extends SApplicative[λ[α => F[G[α]]]] { outer =>
    private implicit def ct[A: ClassTag]: ClassTag[G[A]] = ctgk.deriveClassTag[A]

    override def pure[A: ClassTag](x: A): F[G[A]] = F.pure(G.pure(x))

    override def ap[A, B: ClassTag](fgf: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
      F.ap(F.smap(fgf)(gf => G.ap(gf)(_)))(fga)

    override def product[A, B](fga: F[G[A]], fgb: F[G[B]]): F[G[(A, B)]] =
      F.map2(fga, fgb)(G.product)
  }
}
