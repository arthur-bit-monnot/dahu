package dahu.core.algebra

trait NumberLike[-A] extends Orderable[A] {
  type Num
  def times(a: A, b: A): Num
  def add(a: A, b: A): Num
  def sub(a: A, b: A): Num
  def negate(a: A): Num
}

object NumberLike {
  type Aux[A, C0, Num0] = NumberLike[A] { type Bool = C0; type Num = Num0 }

  def apply[A](implicit ev: NumberLike[A]): Aux[A, ev.Bool, ev.Num] = ev

  class NumberLikeOps[A](private val a: A) extends AnyVal {
    def *(b: A)(implicit N: NumberLike[A]): N.Num = N.times(a, b)
    def +(b: A)(implicit N: NumberLike[A]): N.Num = N.add(a, b)
    def -(b: A)(implicit N: NumberLike[A]): N.Num = N.sub(a, b)
  }

}
