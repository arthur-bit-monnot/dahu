package dahu.model.functions

import dahu.model.types.Value

import dahu.model.types._

abstract class Fun[O: Tag] {
  final val outType: Tag[O] = Tag[O]
  def compute(args: Seq[Value]): O

  def name: String

  override def toString: String = name
}

abstract class Fun1[-I: Tag, O: Tag] extends Fun[O] {
  final val inType = typeOf[I]

  override final def compute(args: Seq[Value]): O = {
    require(args.size == 1)
    of(args(0).asInstanceOf[I])
  }

  def of(in: I): O
}
object Fun1 {
  def embed[A: Tag, B: Tag](f: A => B): Fun1[A, B] = new Fun1[A, B] {
    override def of(in: A): B = f(in)
    override def name: String = f.toString()
  }
}

abstract class Fun2[-I1: Tag, -I2: Tag, O: Tag] extends Fun[O] {
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]

  override final def compute(args: Seq[Value]): O = {
    require(args.size == 2, "Wrong number of arguments, expected 2")
    of(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2])
  }

  def of(in1: I1, in2: I2): O
}

abstract class Fun3[-I1: Tag, -I2: Tag, -I3: Tag, O: Tag] extends Fun[O] {
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]
  final val inType3 = typeOf[I3]

  override final def compute(args: Seq[Value]): O = {
    require(args.size == 3, "Wrong number of arguments, expected 3")
    of(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2], args(2).asInstanceOf[I3])
  }

  def of(in1: I1, in2: I2, in3: I3): O
}

abstract class Fun4[-I1: Tag, -I2: Tag, -I3: Tag, -I4: Tag, O: Tag] extends Fun[O] {
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

abstract class FunN[-I: Tag, O: Tag] extends Fun[O] {
  final val inTypes = typeOf[I]

  override final def compute(args: Seq[Value]): O = of(args.asInstanceOf[Seq[I]])

  def of(args: Seq[I]): O
}

trait WrappedFunction {
  def f: Fun[_]
}

object WrappedFunction {
  def wrap[T, O](f: Fun2[Int, Int, O])(implicit tag: TagIsoInt[T],
                                       outTag: TagIsoInt[O]): Fun2[T, T, O] =
    new WrappedFun2[T, T, O](f)
}

final case class WrappedFun2[I1: TagIsoInt, I2: TagIsoInt, O: TagIsoInt](f: Fun2[Int, Int, O])
    extends Fun2[I1, I2, O]
    with WrappedFunction {
  override def of(in1: I1, in2: I2): O = f.of(TagIsoInt[I1].toInt(in1), TagIsoInt[I2].toInt(in2))
  override def name: String = s"wrapped-${f.name}"
}
