package dahu.model.math

import dahu.geometry.Shape
import dahu.model.functions._
import dahu.model.input.{Cst, Expr}
import dahu.model.ir.CstF
import dahu.model.types._
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

  object Div extends Fun2[Double, Double, Double] {
    override def of(in1: Double, in2: Double): Double = in1 / in2
    override def name: String = "div"
  }

  object Negate extends Fun1[Double, Double] {
    override def name: String = "neg"
    override def of(in: Double): Double = -in
  }

  object Abs extends Fun1[Double, Double] {
    override def name: String = "abs"
    override def of(in: Double): Double = math.abs(in)
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

  object SQRT extends Fun1[Double, Double] {
    override def name: String = "sqrt"
    override def of(in: Double): Double = math.sqrt(in)
  }

  object POW extends Fun2[Double, Double, Double] {
    override def name: String = "pow"
    override def of(base: Double, exponent: Double): Double = math.pow(base, exponent)
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

  class ListBuilder[T: Tag] extends FunN[T, Vec[T]] {
    override def of(args: Seq[T]): Vec[T] = Vec.fromSeq(args)(Tag[T].clazz)
    override def name: String = "list"
  }

  object EQ extends Fun2[Vec[Int], Vec[Int], Bool] {
    override def of(in1: Vec[Int], in2: Vec[Int]): Bool = Bool.asBool(in1 == in2)

    override def name: String = "eq"
  }

  trait Concat[A] extends Monoid[Vec[A]] {}
  def Concat[A: Tag: ClassTag]: Monoid[Vec[A]] = new Monoid[Vec[A]] {
    override def tpe: Tag[Vec[A]] = SequenceTag[A]
    override def combine(lhs: Vec[A], rhs: Vec[A]): Vec[A] = lhs ++ rhs
    override val identity: Vec[A] = Vec.empty[A]
    override def name: String = "concat"
  }

  implicit val tagOfAny: Tag[Any] = Tag.default[Any]
  implicit val tagOfVecAny: Tag[Vec[Any]] = SequenceTag[Any]
  case object Size extends Fun1[Vec[Any], Int] {
    override def of(in: Vec[Any]): Int = in.size
    override def name: String = "size"
  }

  sealed trait Map[I, O] extends Fun2[I ->: O, Vec[I], Vec[O]]
  def Map[I: Tag, O: Tag]: Map[I, O] = new MapImpl[I, O]()

  final private case class MapImpl[I: Tag, O: Tag]() extends Map[I, O] {
    override def of(f: I ->: O, in2: Vec[I]): Vec[O] = in2.map(f.f)(Tag[O].clazz)
    override def name: String = "map"
  }

  final case class Fold[A: Tag](monoid: Monoid[A]) extends Fun1[Vec[A], A] {
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

package object geom {
  implicit val shapeTag = Tag.default[Shape]

  object Point extends Fun2[Double, Double, Shape] {
    override def of(x: Double, y: Double): Shape = Shape.point(x, y)
    override def name: String = "point"
  }

  object Circle extends Fun3[Double, Double, Double, Shape] {
    override def of(x: Double, y: Double, radius: Double): Shape = Shape.circle(x, y, radius)
    override def name: String = "circle"
  }

  object Rectangle extends Fun4[Double, Double, Double, Double, Shape] {
    override def of(x: Double, y: Double, h: Double, w: Double): Shape = Shape.rect(x, y, h, w)
    override def name: String = "rectangle"
  }

  object Polygon extends FunN[Double, Shape] {
    override def of(args: Seq[Double]): Shape = {
      def pairs(xys: List[Double]): List[(Double, Double)] = xys match {
        case x :: y :: tail => (x, y) :: pairs(tail)
        case Nil            => Nil
        case _              => dahu.utils.errors.unexpected("Not a pair numbers of points in polygon")
      }
      Shape.polygon(pairs(args.toList): _*)
    }
    override def name: String = "polygon"
  }

  object SignedDist extends Fun3[Shape, Double, Double, Double] {
    override def of(shape: Shape, x: Double, y: Double): Double = shape.distTo(x, y)
    override def name: String = "signed-dist"
  }
}
