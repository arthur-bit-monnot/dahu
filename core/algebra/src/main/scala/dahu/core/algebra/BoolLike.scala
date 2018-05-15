package dahu.core.algebra

trait GenBoolLike[In, Out] {
  def and(a: In, B: In): Out
  def or(a: In, b: In): Out
  def not(a: In): Out
  def False: Out
  def True: Out
}

trait BoolLike[A] extends GenBoolLike[A, A] {
  def and(a: A, b: A): A
  def or(a: A, b: A): A
  def not(a: A): A
  def implies(a: A, b: A): A = or(not(a), b)
  def False: A
  def True: A
  def andN(as: A*): A = as.foldLeft(True)((acc, cur) => and(acc, cur))
  def orN(as: A*): A = as.foldLeft(False)((acc, cur) => or(acc, cur))
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
