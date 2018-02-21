package dahu.dataframe.vector

trait Vec[F[_]] {
  def empty[A]: F[A]
  def size[A](fa: F[A]): Int

  def at[A](fa: F[A], i: Int): A
  def values[A](fa: F[A]): Iterable[A]

  def updated[A](fa: F[A], i: Int, value: A): F[A]
  def withAppended[A](fa: F[A], value: A): F[A]
}

object Vec {

  def apply[F[_], A](implicit instance: Vec[F]): Vec[F] = instance

  implicit val ofVector: Vec[Vector] = new Vec[Vector] {
    override def empty[A]: Vector[A] = Vector()
    override def size[A](fa: Vector[A]): Int = fa.size

    override def at[A](fa: Vector[A], i: Int): A = fa(i)
    override def values[A](fa: Vector[A]): Iterable[A] = fa

    override def updated[A](fa: Vector[A], i: Int, value: A): Vector[A] =
      fa.updated(i, value)
    override def withAppended[A](fa: Vector[A], value: A): Vector[A] = fa :+ value
  }
}
