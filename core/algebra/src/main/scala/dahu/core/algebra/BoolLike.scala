package dahu.core.algebra

trait BoolLike[A] {
  def and(a: A, b: A): A
  def or(a: A, b: A): A
  def not(a: A): A
}

object BoolLike {

  def apply[A](implicit instance: BoolLike[A]): BoolLike[A] = instance

  class BoolLikeOps[A](private val a: A) extends AnyVal {
    def &&(b: A)(implicit B: BoolLike[A]): A = B.and(a, b)
    def ||(b: A)(implicit B: BoolLike[A]): A = B.or(a, b)
    def unary_!(implicit B: BoolLike[A]): A = B.not(a)
    def implies(b: A)(implicit B: BoolLike[A]): A = B.or(B.not(a), b)
    def ==>(b: A)(implicit B: BoolLike[A]): A = B.or(B.not(a), b)
  }
}
