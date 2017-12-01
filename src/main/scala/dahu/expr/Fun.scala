package dahu.expr

import scala.reflect.runtime.universe._

abstract class Fun[+O : TypeTag] {
  final val outType = typeOf[O]
  def compute(args: Seq[Any]): O

  def name: String

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
    require(args.size == 2, "Wrong number of arguments, expected 2")
    of(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2])
  }
  def of(in1: I1, in2: I2): O
}

abstract class Fun3[-I1 : TypeTag, -I2 : TypeTag, -I3: TypeTag, +O : TypeTag] extends Fun[O] {
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]
  final val inType3 = typeOf[I3]

  override final def compute(args: Seq[Any]): O = {
    require(args.size == 3, "Wrong number of arguments, expected 3")
    of(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2], args(2).asInstanceOf[I3])
  }
  def of(in1: I1, in2: I2, in3: I3): O
}