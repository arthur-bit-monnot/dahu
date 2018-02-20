package dahu

import cats.{Applicative, Eval, Functor, Traverse}
import cats.free.{Cofree, Free}

import scala.language.implicitConversions

package object recursion {

  val Fix: FixModule = FixImpl
  type Fix[F[_]] = Fix.Fix[F]

  type FAlgebra[F[_], A] = F[A] => A
  type FCoalgebra[F[_], A] = A => F[A]

  type FAlgebraM[M[_], F[_], A] = F[A] => M[A]
  type FCoalgebraM[M[_], F[_], A] = A => M[F[A]]

  type RAlgebra[F[_], A] = F[(Fix[F], A)] => A
  type RCoalgebra[F[_], A] = A => F[Either[Fix[F], A]]

  /** Course-of-values algebra */
  type CVAlgebra[F[_], A] = F[Cofree[F, A]] => A

  /** Course-of-values co-algebra */
  type CVCoalgebra[F[_], A] = A => F[Free[F, A]]

  @inline implicit class FixOps[F[_]](private val self: Fix[F]) extends AnyVal {
    @inline def unfix: F[Fix[F]] =
      Fix.unfix(self)
  }

  @inline implicit def fAlgebraOps[F[_], A](self: F[A] => A): FAlgebraOps[F, A] =
    new FAlgebraOps(self)

  @inline implicit def fCoalgebraOps[F[_], A](self: A => F[A]): FCoalgebraOps[F, A] =
    new FCoalgebraOps(self)

  /** EnvT taken from http://codegists.com/snippet/scala/catryoshkascala_andyscott_scala
    * Only modification is the addition of a Functor instance.
    */
  case class EnvT[B, W[_], A](ask: B, lower: W[A])
  object EnvT {
    implicit def envTFunctor[Z, F[_]](implicit F: Functor[F]): Functor[EnvT[Z, F, ?]] =
      new Functor[EnvT[Z, F, ?]] {
        override def map[A, B](fa: EnvT[Z, F, A])(f: A => B): EnvT[Z, F, B] =
        EnvT(fa.ask, F.map(fa.lower)(f))
      }
    implicit def envTTraverse[Z, F[_]](implicit F: Traverse[F]): Traverse[EnvT[Z, F, ?]] =
      new Traverse[EnvT[Z, F, ?]] {
        def traverse[G[_], A, B](fa: EnvT[Z, F, A])(f: A => G[B])(implicit G: Applicative[G]): G[EnvT[Z, F, B]] =
          G.map(F.traverse(fa.lower)(f))(EnvT(fa.ask, _))

        def foldLeft[A, B](fa: EnvT[Z, F, A], b: B)(f: (B, A) => B): B =
          F.foldLeft(fa.lower, b)(f)

        def foldRight[A, B](fa: EnvT[Z, F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          F.foldRight(fa.lower, lb)(f)
      }
  }
}
