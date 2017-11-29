package expr

import scala.reflect.api.TypeTags

final case class Address(address: Long) extends AnyVal

sealed abstract class Value[T]
final case class InMemory[T](address: Address) extends Value[T]
final case class View1[I, O](arg: Value[I], f: Fun1[I,O]) extends Value[O]
final case class Constant[T](value: T) extends Value[T]

import scala.reflect.runtime.universe._

abstract class Fun[+O : TypeTag] {
  final val outType = typeOf[O]
  def compute(args: Seq[Any]): O

  def name: String = super.toString

  override def toString: String = name
}

abstract class Fun1[-I : TypeTag, +O : TypeTag] extends Fun[O] {
  final val inType = typeOf[I]
  override final def compute(args: Seq[Any]): O = {
    require(args.size == 1)
    apply(args(0).asInstanceOf[I])
  }
  def apply(in: I): O
}

abstract class Fun2[-I1 : TypeTag, -I2 : TypeTag, +O : TypeTag] extends Fun[O] {
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]
  override final def compute(args: Seq[Any]): O = {
    require(args.size == 2)
    apply(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2])
  }
  def apply(in1: I1, in2: I2): O
}


object Add extends Fun2[Double, Double, Double] {
  override def name: String = "add"
  override def apply(in1: Double, in2: Double): Double = in1 + in2
}
object Min extends Fun2[Double, Double, Double] {
  override def name: String = "min"
  override def apply(in1: Double, in2: Double): Double = math.min(in1, in2)
}
object LEQ extends Fun2[Double, Double, Boolean] {
  override def name : String = "leq"
  override def apply(in1: Double, in2: Double): Boolean = in1 <= in2
}

