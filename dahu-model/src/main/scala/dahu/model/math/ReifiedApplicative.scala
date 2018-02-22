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

  implicit object ofExpr extends ReifiedApplicative[Expr] {
    override def map[A, Z](fa: Expr[A])(f: Fun1[A, Z]): Expr[Z] =
      Computation(f, fa)

    override def map2[A, B, Z](fa: Expr[A], fb: Expr[B])(f: Fun2[A, B, Z]): Expr[Z] =
      Computation(f, fa, fb)

    override def mapN[A, Z](fas: Expr[A]*)(f: FunN[A, Z]): Expr[Z] =
      Computation(f, fas)

    override def pure[A: Tag](x: A): Expr[A] =
      Cst(x)
  }

  implicit object ofSubjectTo extends ReifiedApplicative[SubjectTo] {
    override def map[A, Z](fa: SubjectTo[A])(f: Fun1[A, Z]): SubjectTo[Z] =
      SubjectTo(ofExpr.map(fa.value)(f), fa.condition)

    override def map2[A, B, Z](fa: SubjectTo[A], fb: SubjectTo[B])(f: Fun2[A, B, Z]): SubjectTo[Z] =
      SubjectTo(ofExpr.map2(fa.value, fb.value)(f),
                ofExpr.mapN(fa.condition, fb.condition)(bool.And))

    override def mapN[A, Z](fas: SubjectTo[A]*)(f: FunN[A, Z]): SubjectTo[Z] =
      SubjectTo(
        ofExpr.mapN(fas.map(_.value): _*)(f),
        ofExpr.mapN(fas.map(_.condition): _*)(bool.And)
      )

    override def pure[A: Tag](x: A): SubjectTo[A] =
      SubjectTo(Cst(x), Cst(true))
  }
}
