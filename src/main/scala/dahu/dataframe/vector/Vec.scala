package dahu.dataframe.vector

trait Vec[F[_], A] {
  def empty: F[A]
  def size(fa: F[A]): Int

  def at(fa: F[A], i: Int): A
  def values(fa: F[A]): Iterable[A]

  def updated(fa: F[A], i: Int, value: A): F[A]
  def withAppended(fa: F[A], value: A): F[A]

}

object Vec {

  def apply[F[_], A](implicit instance: Vec[F, A]): Vec[F, A] = instance

  implicit def ofVector[A]: Vec[Vector, A] = new Vec[Vector, A] {
    override def empty: Vector[A]         = Vector()
    override def size(fa: Vector[A]): Int = fa.size

    override def at(fa: Vector[A], i: Int): A       = fa(i)
    override def values(fa: Vector[A]): Iterable[A] = fa

    override def updated(fa: Vector[A], i: Int, value: A): Vector[A] =
      fa.updated(i, value)
    override def withAppended(fa: Vector[A], value: A): Vector[A] = fa :+ value
  }
}
