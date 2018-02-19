package dahu.model.functions

import dahu.model.types.Value

import dahu.model.types._

abstract class Fun[+O: WTypeTag] {
  final val outType = typeOf[O]
  def compute(args: Seq[Value]): O

  def name: String

  override def toString: String = name
}

abstract class Fun1[-I: WTypeTag, +O: WTypeTag] extends Fun[O] {
  final val inType = typeOf[I]

  override final def compute(args: Seq[Value]): O = {
    require(args.size == 1)
    of(args(0).asInstanceOf[I])
  }

  def of(in: I): O
}

abstract class Fun2[-I1: WTypeTag, -I2: WTypeTag, +O: WTypeTag] extends Fun[O] {
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]

  override final def compute(args: Seq[Value]): O = {
    require(args.size == 2, "Wrong number of arguments, expected 2")
    of(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2])
  }

  def of(in1: I1, in2: I2): O
}

abstract class Fun3[-I1: WTypeTag, -I2: WTypeTag, -I3: WTypeTag, +O: WTypeTag] extends Fun[O] {
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]
  final val inType3 = typeOf[I3]

  override final def compute(args: Seq[Value]): O = {
    require(args.size == 3, "Wrong number of arguments, expected 3")
    of(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2], args(2).asInstanceOf[I3])
  }

  def of(in1: I1, in2: I2, in3: I3): O
}

abstract class Fun4[-I1: WTypeTag, -I2: WTypeTag, -I3: WTypeTag, -I4: WTypeTag, +O: WTypeTag]
    extends Fun[O] {
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]
  final val inType3 = typeOf[I3]
  final val inType4 = typeOf[I4]

  override final def compute(args: Seq[Value]): O = {
    require(args.size == 4, "Wrong number of arguments, expected 3")
    of(args(0).asInstanceOf[I1],
       args(1).asInstanceOf[I2],
       args(2).asInstanceOf[I3],
       args(3).asInstanceOf[I4])
  }

  def of(in1: I1, in2: I2, in3: I3, in4: I4): O
}

abstract class FunN[-I: WTypeTag, +O: WTypeTag] extends Fun[O] {
  final val inTypes = typeOf[I]

  override final def compute(args: Seq[Value]): O = of(args.asInstanceOf[Seq[I]])

  def of(args: Seq[I]): O
}
