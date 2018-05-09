package dahu.core.algebra

trait Orderable[-A] extends Comparable[A, A] {
  override def geq(a: A, b: A): Bool = leq(b, a)
  override def gt(a: A, b: A): Bool = lt(b, a)
}
object Orderable {
  type Aux[A, C0] = Orderable[A] { type Bool = C0 }

  implicit class OrderableOps[A](private val a: A) extends AnyVal {
    def <=(b: A)(implicit O: Orderable[A]): O.Bool = O.leq(a, b)
    def <(b: A)(implicit O: Orderable[A]): O.Bool = O.lt(a, b)
    def >=(b: A)(implicit O: Orderable[A]): O.Bool = O.geq(a, b)
    def >(b: A)(implicit O: Orderable[A]): O.Bool = O.gt(a, b)
    def ===(b: A)(implicit O: Orderable[A]): O.Bool = O.eqv(a, b)
    def =!=(b: A)(implicit O: Orderable[A]): O.Bool = O.neq(a, b)
  }
}
