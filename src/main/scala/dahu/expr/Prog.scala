package dahu.expr

import spire.algebra.Ring

import scala.reflect.runtime.universe.{typeOf, Type, TypeTag}

class If[O: TypeTag] extends Fun3[Boolean, O, O, O] {
  override def name                                = "if"
  override def of(in1: Boolean, in2: O, in3: O): O = if(in1) in2 else in3
}

object Mul extends Fun2[Double, Double, Double] {
  override def name: String                         = "add"
  override def of(in1: Double, in2: Double): Double = in1 * in2
}
object Add extends Fun2[Double, Double, Double] {
  override def name: String                         = "add"
  override def of(in1: Double, in2: Double): Double = in1 + in2
}
object Min extends Fun2[Double, Double, Double] {
  override def name: String                         = "min"
  override def of(in1: Double, in2: Double): Double = math.min(in1, in2)
}
object LEQ extends Fun2[Double, Double, Boolean] {
  override def name: String                          = "leq"
  override def of(in1: Double, in2: Double): Boolean = in1 <= in2
}

class CRingAdd[T: Ring]
