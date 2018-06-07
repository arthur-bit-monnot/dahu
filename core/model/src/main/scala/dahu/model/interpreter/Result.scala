package dahu.model.interpreter

import dahu.utils.{ClassTag, SApplicative}

sealed trait Result[+A] {
  def map[B](f: A => B): Result[B] = this match {
    case Res(v)             => Res(f(v))
    case ConstraintViolated => ConstraintViolated
    case Empty              => Empty
  }
  def flatMap[B](f: A => Result[B]): Result[B] = this match {
    case Res(v)             => f(v)
    case ConstraintViolated => ConstraintViolated
    case Empty              => Empty
  }

}
object Result {

  implicit val applicativeInstance: SApplicative[Result] = new SApplicative[Result] {
    override def pure[A: ClassTag](x: A): Result[A] = Res(x)

    override def ap[A, B: ClassTag](ff: Result[A => B])(fa: Result[A]): Result[B] =
      (ff, fa) match {
        case (Empty, _)              => Empty
        case (_, Empty)              => Empty
        case (ConstraintViolated, _) => ConstraintViolated
        case (_, ConstraintViolated) => ConstraintViolated
        case (Res(f), Res(a))        => Res(f(a))
      }
  }
}

final case object ConstraintViolated extends Result[Nothing]
final case class Res[T](v: T) extends Result[T]
final case object Empty extends Result[Nothing]
