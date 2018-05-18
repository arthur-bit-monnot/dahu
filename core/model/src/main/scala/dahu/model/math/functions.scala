package dahu.model.math

import dahu.model.functions._
import dahu.model.input.{Cst, Expr}
import dahu.model.ir.CstF
import dahu.model.types.Tag

object double {

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

  object LEQ extends Fun2[Double, Double, Boolean] {
    override def name: String = "leq"
    override def of(in1: Double, in2: Double): Boolean = in1 <= in2
  }
  object EQ extends Fun2[Double, Double, Boolean] {
    override def name: String = "eq"
    override def of(in1: Double, in2: Double): Boolean = in1 == in2
  }
  object LT extends Fun2[Double, Double, Boolean] {
    override def name: String = "lt"
    override def of(in1: Double, in2: Double): Boolean = in1 < in2
  }

}

object int {

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

  object LEQ extends Fun2[Int, Int, Boolean] {
    override def name: String = "leq"
    override def of(in1: Int, in2: Int): Boolean = in1 <= in2
  }
  object EQ extends Fun2[Int, Int, Boolean] {
    override def name: String = "eq"
    override def of(in1: Int, in2: Int): Boolean = in1 == in2
  }
}

object bool {

  object And extends CommutativeMonoid[Boolean] with IdempotentMonoid[Boolean] {
    override def tpe: Tag[Boolean] = Tag.ofBoolean
    override def name: String = "and"
    override def combine(lhs: Boolean, rhs: Boolean): Boolean = lhs && rhs
    override val identity: Boolean = true
  }
  object Or extends CommutativeMonoid[Boolean] with IdempotentMonoid[Boolean] {
    override def tpe: Tag[Boolean] = Tag.ofBoolean
    override def name: String = "or"
    override def combine(lhs: Boolean, rhs: Boolean): Boolean = lhs || rhs
    override val identity: Boolean = false
  }
  object XOr extends FunN[Boolean, Boolean] {
    override def of(args: Seq[Boolean]): Boolean = args.count(_ == true) == 1
    override def name: String = "xor"
  }

  object Not extends Reversible[Boolean, Boolean] {
    override def name: String = "not"
    override def of(in: Boolean): Boolean = !in
    override def reverse: Reversible[Boolean, Boolean] = this
  }

  val True: Expr[Boolean] = Cst(true)
  val TrueF: CstF[Any] = CstF(dahu.model.types.Value(true), Tag[Boolean])
  val False: Expr[Boolean] = Cst(false)
  val FalseF: CstF[Any] = CstF(dahu.model.types.Value(false), Tag[Boolean])

}
