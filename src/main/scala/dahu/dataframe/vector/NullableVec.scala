package dahu.dataframe.vector

trait NullableVec[F[_]] {

  def empty[A]: F[A]
  def init[A](size: Int): F[A]
  def size[A](fa: F[A]): Int


  def get[A](fa: F[A], i: Int): Option[A]
  def getUnsafe[A](fa: F[A], i: Int): A
  def getOrElse[A](fa: F[A], i: Int, default: => A): A = get(fa, i).getOrElse(default)
  def isSet[A](fa: F[A], i: Int): Boolean

  def values[A](fa: F[A]): Iterable[Option[A]]

  def updated[A](fa: F[A], i: Int, value: A): F[A]
  def withAppended[A](fa: F[A], value: A): F[A]

}

object NullableVec {

  implicit val ofVector: NullableVec[Vector] = new NullableVec[Vector] {override def withAppended[A](fa: Vector[A], value: A): Vector[A] = ???

    override def isSet[A](fa: Vector[A], i: Int): Boolean = fa(i) != null

    override def values[A](fa: Vector[A]): Iterable[Option[A]] = fa.map(Option(_))

    override def empty[A]: Vector[A] = Vector[A]()

    override def size[A](fa: Vector[A]): Int = fa.size

    override def init[A](size: Int): Vector[A] = Vector.fill[A](size)(null.asInstanceOf[A])

    override def get[A](fa: Vector[A], i: Int): Option[A] = Option(fa(i))

    override def updated[A](fa: Vector[A], i: Int, value: A): Vector[A] = fa.updated(i, value)

    override def getUnsafe[A](fa: Vector[A], i: Int): A = fa(i)
  }

}


