package dahu.model.math

import dahu.model.functions.{Fun1, Fun2, FunN}
import dahu.model.input._
import dahu.model.types.Tag

trait ReifiedApplicative[F[_]] {

  def map[A, Z](fa: F[A])(f: Fun1[A, Z]): F[Z]
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: Fun2[A, B, Z]): F[Z]
  def mapN[A, Z](fa: F[A]*)(f: FunN[A, Z]): F[Z]
  def pure[A: Tag](x: A): F[A]
}

object ReifiedApplicative {

  implicit object ofTentative extends ReifiedApplicative[Tentative] {
    override def map[A, Z](fa: Tentative[A])(f: Fun1[A, Z]): Tentative[Z] =
      Computation(f, fa)

    override def map2[A, B, Z](fa: Tentative[A], fb: Tentative[B])(f: Fun2[A, B, Z]): Tentative[Z] =
      Computation(f, fa, fb)

    override def mapN[A, Z](fas: Tentative[A]*)(f: FunN[A, Z]): Tentative[Z] =
      Computation(f, fas)

    override def pure[A: Tag](x: A): Tentative[A] =
      Cst(x)
  }
}
