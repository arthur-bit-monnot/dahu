package dahu.model.math

import dahu.model.functions.{Fun1, Fun2, Fun3, FunN}
import dahu.model.types.WTypeTag

object double {

  object Times extends Fun2[Double, Double, Double] {
    override def name: String = "add"

    override def of(in1: Double, in2: Double): Double = in1 * in2
  }

  object Add extends Fun2[Double, Double, Double] {
    override def name: String = "add"

    override def of(in1: Double, in2: Double): Double = in1 + in2
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

}

object int {

  object Times extends Fun2[Int, Int, Int] {
    override def name: String = "add"
    override def of(in1: Int, in2: Int): Int = in1 * in2
  }

  object Add extends Fun2[Int, Int, Int] {
    override def name: String = "add"
    override def of(in1: Int, in2: Int): Int = in1 + in2
  }

  object Negate extends Fun1[Int, Int] {
    override def name: String = "neg"
    override def of(in: Int): Int = -in
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
  object NEQ extends Fun2[Int, Int, Boolean] {
    override def name: String = "neq"
    override def of(in1: Int, in2: Int): Boolean = in1 != in2
  }

}

object bool {

  object And extends FunN[Boolean, Boolean] {
    override def name: String = "and"
    override def of(args: Seq[Boolean]): Boolean = args.forall(identity)
  }
  object Or extends FunN[Boolean, Boolean] {
    override def name: String = "or"
    override def of(args: Seq[Boolean]): Boolean = args.exists(identity)
  }

  class If[O: WTypeTag] extends Fun3[Boolean, O, O, O] {
    override def name = "if"
    override def of(in1: Boolean, in2: O, in3: O): O = if(in1) in2 else in3
  }

  object Not extends Fun1[Boolean, Boolean] {
    override def name: String = "not"
    override def of(in: Boolean): Boolean = !in
  }

}
