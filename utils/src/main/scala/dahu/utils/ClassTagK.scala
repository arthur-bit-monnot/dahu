package dahu.utils
import cats.Id

trait ClassTagK[F[_]] {

  def deriveClassTag[A: ClassTag]: ClassTag[F[A]]

}

object ClassTagK {

  def apply[F[_]](implicit ev: ClassTagK[F]): ClassTagK[F] = ev

  implicit def deriveClassTag[F[_]: ClassTagK, A: ClassTag]: ClassTag[F[A]] = {
    implicitly[ClassTagK[F]].deriveClassTag[A]
  }

  private class Witness

  /** If we have a evidence that F[A] A AnyRef, we infer that F[_] denotes a identity type
    * An return the class tag of A when asked for the class tag of F[A] */
  implicit def ofIdentity[F[_]](implicit ev: F[Witness] =:= Witness): ClassTagK[F] =
    new ClassTagK[F] {
      override def deriveClassTag[A: ClassTag]: ClassTag[F[A]] =
        implicitly[ClassTag[A]].asInstanceOf[ClassTag[F[A]]]
    }

  /** If we have a evidence that F[Int] <:< AnyRef, we infer that F[_] denotes a class
    * Whose type parameter is erased and that it's classtag is invariant in its parameter */
  implicit def ofWrapper[F[_]](implicit ct: ClassTag[F[Any]], ev: F[Int] <:< AnyRef): ClassTagK[F] =
    new ClassTagK[F] {
      override def deriveClassTag[A: ClassTag]: ClassTag[F[A]] = ct.asInstanceOf[ClassTag[F[A]]]
    }
}
