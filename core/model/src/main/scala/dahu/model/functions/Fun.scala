package dahu.model.functions

import dahu.utils._
import dahu.model.types.Value
import dahu.model.types._

trait FunAny {

  /** Returns true if the function should never be executed as is. */
  def isMacro: Boolean = false
  def arity: Option[Int]
  def compute(args: Vec[Value]): Any
  def computeFromAny(args: Vec[Any]): Any = compute(args.asInstanceOf[Vec[Value]])

  def name: String
  def outType: TagAny

  def funType: LambdaTagAny
}

abstract class Fun[O: Tag] extends FunAny {
  def outType: Tag[O] = Tag[O]
  require(outType != null)
  def compute(args: Vec[Value]): O
  override def computeFromAny(args: Vec[Any]): O = compute(args.asInstanceOf[Vec[Value]])

  def name: String

  override def toString: String = name
}

abstract class Fun1[-I: Tag, O: Tag] extends Fun[O] with (I ->: O) {
  override def arity: Option[Int] = Some(1)
  final val inType = typeOf[I]

  override final def compute(args: Vec[Value]): O = {
    require(args.size == 1)
    of(args(0).asInstanceOf[I])
  }

  def of(in: I): O

  override def underlyingFunction: I => O = of

  override final def funType: LambdaTagAny = LambdaTag.derive[I, O]
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
    override def of(in: A): B = underlyingFunction(in)
    override def name: String = underlyingFunction.toString()
  }
}

abstract class Fun2[-I1: Tag, -I2: Tag, O: Tag] extends Fun[O] with (I1 ->: I2 ->: O) {
  override def arity: Option[Int] = Some(2)
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]

  override final def compute(args: Vec[Value]): O = {
    require(args.size == 2, "Wrong number of arguments, expected 2")
    of(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2])
  }

  def of(in1: I1, in2: I2): O

  override def underlyingFunction: I1 => (I2 ->: O) =
    in1 => lift(in2 => of(in1, in2))

  override final def funType: LambdaTagAny = LambdaTag.derive[I1, I2 ->: O]
}

abstract class Fun3[-I1: Tag, -I2: Tag, -I3: Tag, O: Tag] extends Fun[O] {
  override def arity: Option[Int] = Some(3)
  final val inType1 = typeOf[I1]
  final val inType2 = typeOf[I2]
  final val inType3 = typeOf[I3]

  override final def compute(args: Vec[Value]): O = {
    require(args.size == 3, "Wrong number of arguments, expected 3")
    of(args(0).asInstanceOf[I1], args(1).asInstanceOf[I2], args(2).asInstanceOf[I3])
  }

  def of(in1: I1, in2: I2, in3: I3): O

  override final def funType: LambdaTagAny =
    LambdaTag.of(
      Tag[I1],
      LambdaTag.of(Tag[I2], LambdaTag.of(Tag[I3], Tag[O]))
    )
}

abstract class Fun4[-I1: Tag, -I2: Tag, -I3: Tag, -I4: Tag, O: Tag] extends Fun[O] {
  override def arity: Option[Int] = Some(3)
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

  override final def funType: LambdaTagAny =
    LambdaTag.of(
      Tag[I1],
      LambdaTag.of(Tag[I2], LambdaTag.of(Tag[I3], LambdaTag.of(Tag[I4], Tag[O])))
    )
}

abstract class FunN[-I: Tag, O: Tag] extends Fun[O] {
  override def arity: Option[Int] = None
  final val inTypes = typeOf[I]

  override final def compute(args: Vec[Value]): O =
    of(args.toSeq.asInstanceOf[Seq[I]]) //TODO: avoid conversion

  def of(args: Seq[I]): O //TODO

  override final def funType: LambdaTagAny = LambdaTag.derive[Vec[I], O]
}
