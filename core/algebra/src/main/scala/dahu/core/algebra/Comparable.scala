package dahu.core.algebra

trait Comparable[-A, -B] {
  type EBool
  def BL: BoolLike[EBool]
  def leq(a: A, b: B): EBool
  def lt(a: A, b: B): EBool
  def geq(a: A, b: B): EBool
  def gt(a: A, b: B): EBool
  def eqv(a: A, b: B): EBool = BL.and(leq(a, b), geq(a, b))
  def neq(a: A, b: B): EBool = BL.not(eqv(a, b))
}

object Comparable {

  type Aux[A, B, C0] = Comparable[A, B] { type EBool = C0 }

}
