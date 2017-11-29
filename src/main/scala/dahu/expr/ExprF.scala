package dahu.expr

import matryoshka._

import matryoshka.implicits._

sealed abstract class Arithmetic
final case class Number(v: Double) extends Arithmetic
final case class Add(a: Arithmetic, b: Arithmetic) extends Arithmetic
final case class Subtract(a: Arithmetic, b: Arithmetic) extends Arithmetic
final case class Multiply(a: Arithmetic, b: Arithmetic) extends Arithmetic
final case class Divide(a: Arithmetic, b: Arithmetic) extends Arithmetic

sealed abstract class ArithmeticF[A]
final case class NumberF[A](v: Double) extends ArithmeticF[A]
final case class AddF[A](a: A, b: A) extends ArithmeticF[A]
final case class SubtractF[A](a: A, b: A) extends ArithmeticF[A]
final case class MultiplyF[A](a: A, b: A) extends ArithmeticF[A]
final case class DivideF[A](a: A, b: A) extends ArithmeticF[A]

object Main extends App {

  val arithmeticCoalgebra: Coalgebra[ArithmeticF, Arithmetic] = {
    case Number(v)      => NumberF(v)
    case Add(a, b)      => AddF(a, b)
    case Subtract(a, b) => SubtractF(a, b)
    case Multiply(a, b) => MultiplyF(a, b)
    case Divide(a, b)   => DivideF(a, b)
  }

  val arithmeticAlgebra: Algebra[ArithmeticF, Arithmetic] = {
    case NumberF(v)      => Number(v)
    case AddF(a, b)      => Add(a, b)
    case SubtractF(a, b) => Subtract(a, b)
    case MultiplyF(a, b) => Multiply(a, b)
    case DivideF(a, b)   => Divide(a, b)
  }

  import scalaz.{Divide => _, _}, Scalaz._

  implicit val traverse: Traverse[ArithmeticF] = new Traverse[ArithmeticF] {
    def traverseImpl[G[_], A, B]
      (fa: ArithmeticF[A])
      (f: A => G [B])
      (implicit G: Applicative[G]) =
      fa match {
        case NumberF(v)      => G.point(NumberF(v))
        case AddF(a, b)      => (f(a) ⊛ f(b))(AddF(_, _))
        case SubtractF(a, b) => (f(a) ⊛ f(b))(SubtractF(_, _))
        case MultiplyF(a, b) => (f(a) ⊛ f(b))(MultiplyF(_, _))
        case DivideF(a, b)   => (f(a) ⊛ f(b))(DivideF(_, _))
      }
  }

  implicit val arithmeticBirecursive: Birecursive.Aux[Arithmetic, ArithmeticF] =
    Birecursive.fromAlgebraIso(arithmeticAlgebra, arithmeticCoalgebra)


  val pprint: Algebra[ArithmeticF, String] = {
    case NumberF(v)      => v.toString
    case AddF(a, b)      => s"($a) + ($b)"
    case SubtractF(a, b) => s"($a) - ($b)"
    case MultiplyF(a, b) => s"($a) * ($b)"
    case DivideF(a, b)   => s"($a) / ($b)"
  }
  val eval: Algebra[ArithmeticF, Double] = {
    case NumberF(v) => v
    case AddF(a, b) => a + b
    case SubtractF(a, b) => a - b
    case MultiplyF(a, b) => a * b
    case DivideF(a, b) => a / b
  }

  val precedence: ArithmeticF[_] => Int = {
    case NumberF(_)      => 0
    case AddF(_, _)      => 3
    case SubtractF(_, _) => 4
    case MultiplyF(_, _) => 1
    case DivideF(_, _)   => 2
  }

  def buildOp
    (currentPrecedence: Int, a: (Int, String), op: String, b: (Int, String))
      : String = {
    val newA = if (a._1 <= currentPrecedence) a._2 else s"(${a._2})"
    val newB = if (b._1 < currentPrecedence) b._2 else s"(${b._2})"

    s"$newA $op $newB"
  }

  val pprint2: GAlgebra[(Int, ?), ArithmeticF, String] = {
    case NumberF(v)      => v.toString
    case AddF(a, b)      => buildOp(3, a, "+", b)
    case SubtractF(a, b) => buildOp(4, a, "-", b)
    case MultiplyF(a, b) => buildOp(1, a, "*", b)
    case DivideF(a, b)   => buildOp(2, a, "/", b)
  }

  val expr: Arithmetic =
    Add(Multiply(Number(3), Divide(Number(4), Number(5))), Number(6))

  //  println(arithmeticCoalgebra.apply(expr).cata(pprint))
  println(Recursive[Arithmetic].cata(expr)(pprint))
  println(Recursive[Arithmetic].cata(expr)(eval))

  println(Recursive[Arithmetic].zygo(expr)(precedence, pprint2))
  println("coucou")
}

