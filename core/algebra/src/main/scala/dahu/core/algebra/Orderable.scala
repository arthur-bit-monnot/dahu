package dahu.core.algebra

trait Orderable[-A] extends Comparable[A, A] {
  override def geq(a: A, b: A): EBool = leq(b, a)
  override def gt(a: A, b: A): EBool = lt(b, a)
}
object Orderable {
  type Aux[A, C0] = Orderable[A] { type EBool = C0 }

  implicit class OrderableOps[A](private val a: A) extends AnyVal {
    def <=(b: A)(implicit O: Orderable[A]): O.EBool = O.leq(a, b)
    def <(b: A)(implicit O: Orderable[A]): O.EBool = O.lt(a, b)
    def >=(b: A)(implicit O: Orderable[A]): O.EBool = O.geq(a, b)
    def >(b: A)(implicit O: Orderable[A]): O.EBool = O.gt(a, b)
    def ===(b: A)(implicit O: Orderable[A]): O.EBool = O.eqv(a, b)
    def =!=(b: A)(implicit O: Orderable[A]): O.EBool = O.neq(a, b)
  }
}
