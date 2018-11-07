package dahu.model.math

import dahu.geometry.Shape
import dahu.model.functions._
import dahu.model.input.{Cst, Expr}
import dahu.model.ir.CstF
import dahu.model.products.ProductTagAny
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
    override def outType: SequenceTag[T] = SequenceTag.apply[T]
  }

  object EQ extends Fun2[Vec[Int], Vec[Int], Bool] {
    override def of(in1: Vec[Int], in2: Vec[Int]): Bool = Bool.asBool(in1 == in2)
    override def name: String = "eq"
  }

  trait Concat[A] extends Monoid[Vec[A]] {}
  def Concat[A: Tag]: Concat[A] = new Concat[A] {
    private implicit def clazzTag: ClassTag[A] = Tag[A].clazz
    override def tpe: Tag[Vec[A]] = SequenceTag[A]
    override def combine(lhs: Vec[A], rhs: Vec[A]): Vec[A] = lhs ++ rhs
    override val identity: Vec[A] = Vec.empty[A]
    override def name: String = "concat"
  }

  implicit val tagOfAny: Tag[Any] = Tag.unsafe.ofAny
  implicit val tagOfVecAny: Tag[Vec[Any]] = SequenceTag[Any]
  case object Size extends Fun1[Vec[Any], Int] {
    override def of(in: Vec[Any]): Int = in.size
    override def name: String = "size"
  }

  sealed trait Map[I, O] extends Fun2[I ->: O, Vec[I], Vec[O]]
  def Map[I: Tag, O: Tag]: Map[I, O] = new MapImpl[I, O]()

  final private case class MapImpl[I: Tag, O: Tag]() extends Map[I, O] {
    override def of(f: I ->: O, in2: Vec[I]): Vec[O] = in2.map(f.underlyingFunction)(Tag[O].clazz)
    override def name: String = "map"
  }

  final case class Fold[A: Tag](monoid: Monoid[A]) extends Fun1[Vec[A], A] {
    override def of(in: Vec[A]): A = in.foldLeft(monoid.identity)((a, b) => monoid.combine(a, b))
    override def name: String = s"fold($monoid)"
  }

  sealed trait Last[I] extends Fun1[Vec[I], I]
  def Last[I: Tag]: Last[I] = LastImpl()

  final private case class LastImpl[I: Tag]() extends Last[I] {
    override def of(in: Vec[I]): I = in.lastUnsafe
    override def name: String = "last"
  }

  sealed trait First[I] extends Fun1[Vec[I], I]
  def First[I: Tag]: First[I] = FirstImpl()

  final private case class FirstImpl[I: Tag]() extends First[I] {
    override def of(in: Vec[I]): I = in.firstUnsafe
    override def name: String = "first"
  }

  sealed abstract class AllConsecutive[I: Tag]
      extends Fun2[Vec[I], I ->: I ->: Bool, Bool]()(Tag.ofSequence[I],
                                                     Tag.ofFunction2[I, I, Bool],
                                                     Tag.ofBoolean)
  def AllConsecutive[I: Tag]: AllConsecutive[I] = AllConsecutiveImpl[I]()

  final private case class AllConsecutiveImpl[I: Tag]() extends AllConsecutive[I] {
    override def of(vec: Vec[I], f: I ->: I ->: Bool): Bool = {
      if(vec.length <= 1)
        Bool.True
      else {
        for(i <- 0 until (vec.length - 1)) {
          val a = vec(i)
          val b = vec(i + 1)
          val valid = f.eval(a).eval(b)
          if(valid == Bool.False)
            return Bool.False
        }
        Bool.True
      }
    }
    override def name: String = "forall-consecutive"
  }

  sealed trait FirstMatches[I] extends Fun2[Vec[I], I ->: Bool, Bool]
  def FirstMatches[I: Tag]: FirstMatches[I] = FirstMatchesImpl()

  final private case class FirstMatchesImpl[I: Tag]() extends FirstMatches[I] {
    override def of(in: Vec[I], predicate: I ->: Bool): Bool =
      if(in.size > 0)
        predicate.underlyingFunction(in.firstUnsafe)
      else
        Bool.False
    override def name: String = "first-matches"
  }

  sealed trait LastMatches[I] extends Fun2[Vec[I], I ->: Bool, Bool]
  def LastMatches[I: Tag]: LastMatches[I] = LastMatchesImpl()

  final private case class LastMatchesImpl[I: Tag]() extends LastMatches[I] {
    override def of(in: Vec[I], predicate: I ->: Bool): Bool =
      if(in.size > 0)
        predicate.underlyingFunction(in.lastUnsafe)
      else
        Bool.False
    override def name: String = "first-matches"
  }
}

package object any {

  import Tag.unsafe.ofAny

  sealed trait EQ

  private object EQSingleton extends Fun2[Any, Any, Bool] with EQ {
    override def of(in1: Any, in2: Any): Bool = Bool.asBool(in1 == in2)
    override def name: String = "any-eq"
  }
  def EQ[T]: Fun2[T, T, Bool] = EQSingleton

  object TypeOf extends Fun1[Any, TagAny] {
    override def isMacro: Boolean = true
    override def of(in: Any): TagAny =
      dahu.utils.errors.unexpected("TypeOf should be compile time only")
    override def name: String = "type-of"
  }
}

package object products {

  private implicit val funAnyTag = Tag.default[FunAny]

  object Extractor extends Fun2[String, TagAny, FunAny] {
    override def of(in1: String, in2: TagAny): FunAny = in2 match {
      case prod: ProductTagAny =>
        prod.getAccessorAny(in1) match {
          case Some(accessor) => accessor
          case None           => dahu.utils.errors.unexpected(s"No field named $in1 in type $prod")
        }
      case tpe => dahu.utils.errors.unexpected(s"Type $tpe is not a product type ")
    }
    override def name: String = "extractor"
  }

}

package object geom {
  implicit val shapeTag: Tag[Shape] = Tag.default[Shape]

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
