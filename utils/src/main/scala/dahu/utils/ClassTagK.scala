package dahu.utils
import cats.Id

trait ClassTagK[F[_]] {

  def deriveClassTag[A: ClassTag]: ClassTag[F[A]]

}

object ClassTagK {

  implicit object idInstance extends ClassTagK[cats.Id] {
    override def deriveClassTag[A: ClassTag]: ClassTag[Id[A]] =
      implicitly[ClassTag[A]]
  }

  def ofClass[F[_]](implicit ct: ClassTag[F[Any]]): ClassTagK[F] = new ClassTagK[F] {
    override def deriveClassTag[A: ClassTag]: ClassTag[F[A]] = ct.asInstanceOf[ClassTag[F[A]]]
  }
}
