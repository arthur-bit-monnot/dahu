package dahu.core.algebra

trait Comparable[-A, -B] {
  type Bool
  def BL: BoolLike[Bool]
  def leq(a: A, b: B): Bool
  def lt(a: A, b: B): Bool
  def geq(a: A, b: B): Bool
  def gt(a: A, b: B): Bool
  def eqv(a: A, b: B): Bool = BL.and(leq(a, b), geq(a, b))
  def neq(a: A, b: B): Bool = BL.not(eqv(a, b))
}

object Comparable {

  type Aux[A, B, C0] = Comparable[A, B] { type Bool = C0 }

}
