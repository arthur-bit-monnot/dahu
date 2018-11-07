package dahu.model

package object functions {

  trait ->:[-I, +O] {
    def underlyingFunction: I => O

    def eval(i: I): O = underlyingFunction(i)
  }

  def lift[I, O](f: I => O): I ->: O = new ->:[I, O] {
    override def underlyingFunction: I => O = f
  }
//  final case class ->:[I, O](f: I => O) {
//    def apply(i: I): O = f(i)
//  }

}
