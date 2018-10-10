package dahu.model.math

import dahu.model.functions._
import dahu.model.input.{Cst, Expr}
import dahu.model.ir.CstF
import dahu.model.types.{Bool, ProductTag, SequenceTag, Tag}
import dahu.utils.Vec

import scala.reflect.ClassTag

package double {

  object Times extends CommutativeMonoid[Double] {
    override def tpe: Tag[Double] = Tag.ofDouble
    override def name: String = "times"
    override def combine(lhs: Double, rhs: Double): Double = lhs * rhs
    override val identity: Double = 1
  }

  object Add extends CommutativeMonoid[Double] {
    override def tpe: Tag[Double] = Tag.ofDouble
    override def name: String = "add"
    override def combine(lhs: Double, rhs: Double): Double = lhs + rhs
    override val identity: Double = 0
  }

  object Negate extends Fun1[Double, Double] {
    override def name: String = "neg"
    override def of(in: Double): Double = -in
  }

  object Min extends Fun2[Double, Double, Double] {
    override def name: String = "min"

    override def of(in1: Double, in2: Double): Double = math.min(in1, in2)
  }

  object LEQ extends Fun2[Double, Double, Bool] {
    override def name: String = "leq"
    override def of(in1: Double, in2: Double): Bool = Bool.asBool(in1 <= in2)
  }
  object EQ extends Fun2[Double, Double, Bool] {
    override def name: String = "eq"
    override def of(in1: Double, in2: Double): Bool = Bool.asBool(in1 == in2)
  }
  object LT extends Fun2[Double, Double, Bool] {
    override def name: String = "lt"
    override def of(in1: Double, in2: Double): Bool = Bool.asBool(in1 < in2)
  }

}

package int {

  object Times extends CommutativeMonoid[Int] {
    override def tpe: Tag[Int] = Tag.ofInt
    override def name: String = "times"
    override def combine(lhs: Int, rhs: Int): Int = lhs * rhs
    override val identity: Int = 1
  }

  object Add extends CommutativeMonoid[Int] {
    override def tpe: Tag[Int] = Tag.ofInt
    override def name: String = "add"
    override def combine(lhs: Int, rhs: Int): Int = lhs + rhs
    override val identity: Int = 0
  }

  object Negate extends Reversible[Int, Int] {
    override def name: String = "neg"
    override def of(in: Int): Int = -in
    override def reverse: Reversible[Int, Int] = this
  }

  object Min extends Fun2[Int, Int, Int] {
    override def name: String = "min"
    override def of(in1: Int, in2: Int): Int = math.min(in1, in2)
  }

  object LEQ extends Fun2[Int, Int, Bool] {
    override def name: String = "leq"
    override def of(in1: Int, in2: Int): Bool = Bool.asBool(in1 <= in2)
  }
  object EQ extends Fun2[Int, Int, Bool] {
    override def name: String = "eq"
    override def of(in1: Int, in2: Int): Bool = Bool.asBool(in1 == in2)
  }
}

package bool {
  import Bool._

  object And extends CommutativeMonoid[Bool] with IdempotentMonoid[Bool] {
    override def tpe: Tag[Bool] = Tag.ofBoolean
    override def name: String = "and"
    override def combine(lhs: Bool, rhs: Bool): Bool = lhs && rhs
    override val identity: Bool = Bool.True
  }
  object Or extends CommutativeMonoid[Bool] with IdempotentMonoid[Bool] {
    override def tpe: Tag[Bool] = Tag.ofBoolean
    override def name: String = "or"
    override def combine(lhs: Bool, rhs: Bool): Bool = lhs || rhs
    override val identity: Bool = Bool.False
  }
  object XOr extends CommutativeMonoid[Bool] {
    override def tpe: Tag[Bool] = Tag.ofBoolean
    override def name: String = "xor"
    override def combine(lhs: Bool, rhs: Bool): Bool = if(lhs + rhs == 1) Bool.True else Bool.False
    override val identity: Bool = Bool.False
  }

  object Not extends Reversible[Bool, Bool] {
    override def name: String = "not"
    override def of(in: Bool): Bool = !in
    override def reverse: Reversible[Bool, Bool] = this
  }
}
package object bool {
  final val True: Expr[Bool] = Cst[Bool](Bool.True)
  final val TrueF: CstF[Any] = CstF(dahu.model.types.Value(Bool.True), Tag.ofBoolean)
  final val False: Expr[Bool] = Cst(Bool.False)
  final val FalseF: CstF[Any] = CstF(dahu.model.types.Value(Bool.False), Tag.ofBoolean)
}

package object sequence {

  object EQ extends Fun2[Vec[Int], Vec[Int], Boolean] {
    override def of(in1: Vec[Int], in2: Vec[Int]): Boolean = in1 == in2

    override def name: String = "eq"
  }

  trait Concat[A] extends Monoid[Vec[A]] {}
  def Concat[A: Tag: ClassTag]: Monoid[Vec[A]] = new Monoid[Vec[A]] {
    override def tpe: Tag[Vec[A]] = SequenceTag[A]
    override def combine(lhs: Vec[A], rhs: Vec[A]): Vec[A] = lhs ++ rhs
    override val identity: Vec[A] = Vec.empty[A]
    override def name: String = "concat"
  }

  final case class Map[I: Tag, O: Tag: ClassTag](f: Fun1[I, O])
      extends Fun1[Vec[I], Vec[O]]()(SequenceTag[I], SequenceTag[O]) {
    override def of(in: Vec[I]): Vec[O] = in.map(f.of)
    override def name: String = s"map(${f.name})"
  }

  final case class Fold[A: Tag: ClassTag](monoid: Monoid[A]) extends Fun1[Vec[A], A] {
    override def of(in: Vec[A]): A = in.foldLeft(monoid.identity)((a, b) => monoid.combine(a, b))
    override def name: String = s"fold($monoid)"
  }
}

package object any {

  private[this] implicit val anyTag: Tag[Any] = Tag.default[Any]

  sealed trait EQ

  private object EQSingleton extends Fun2[Any, Any, Bool] with EQ {
    override def of(in1: Any, in2: Any): Bool = Bool.asBool(in1 == in2)
    override def name: String = "any-eq"
  }
  def EQ[T]: Fun2[T, T, Bool] = EQSingleton
}
