package dahu.model.functions

import dahu.utils._
import dahu.model.types.Value
import dahu.model.types._

trait FunAny {
  def compute(args: Vec[Value]): Any
  def computeFromAny(args: Vec[Any]): Any

  def name: String
}

abstract class Fun[O: Tag] extends FunAny {
  final val outType: Tag[O] = Tag[O]
  require(outType != null)
  def compute(args: Vec[Value]): O
  def computeFromAny(args: Vec[Any]): O = compute(args.asInstanceOf[Vec[Value]])

  def name: String

  override def toString: String = name
}

abstract class Fun1[-I: Tag, O: Tag] extends Fun[O] {
  final val inType = typeOf[I]

  override final def compute(args: Vec[Value]): O = {
    require(args.size == 1)
    of(args(0).asInstanceOf[I])
  }

  def of(in: I): O
}

abstract class Reversible[A: Tag, B: Tag] extends Fun1[A, B] {
  def reverse: Reversible[B, A]
}
sealed abstract class Box[T: TagIsoInt] extends Reversible[Int, T] {
  def reverse: Unbox[T]
}
final class Unbox[T: TagIsoInt] extends Reversible[T, Int] { self =>
  def tag: TagIsoInt[T] = TagIsoInt[T]
  override val reverse: Box[T] = new Box[T] {
    override def reverse: Unbox[T] = self
    override def of(in: Int): T = tag.fromInt(in)
    override def name: String = "box"
  }
  override def of(in: T): Int = tag.toInt(in)
  override def name: String = "unbox"
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

  override final def compute(args: Vec[Value]): O = {
    require(args.size == 2, "Wrong number of arguments, expected 2")
    of(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2])
  }

  def of(in1: I1, in2: I2): O
}

abstract class Fun3[-I1: Tag, -I2: Tag, -I3: Tag, O: Tag] extends Fun[O] {
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]
  final val inType3 = typeOf[I3]

  override final def compute(args: Vec[Value]): O = {
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

  override final def compute(args: Vec[Value]): O = {
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

  override final def compute(args: Vec[Value]): O =
    of(args.toSeq.asInstanceOf[Seq[I]]) //TODO: avoid conversion

  def of(args: Seq[I]): O //TODO
}
