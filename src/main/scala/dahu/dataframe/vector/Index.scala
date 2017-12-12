package dahu.dataframe.vector

trait Index[F[_], A] {

  def contains(fa: F[A], a: A): Boolean
  def idUnsafe(fa: F[A], a: A): Int

  def id(fa: F[A], a: A): Option[Int] =
    if(contains(fa, a))
      Some(idUnsafe(fa, a))
    else
      None

}
