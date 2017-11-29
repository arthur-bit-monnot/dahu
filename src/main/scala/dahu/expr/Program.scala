package dahu.expr

import expr._
import matryoshka.{Algebra, Coalgebra, Recursive}

import scala.reflect.runtime.universe._

sealed abstract class Result[T : TypeTag] {
  final val typ = typeOf[T]
}
final case class Input[T : TypeTag](name: String) extends Result[T]
final case class Cst[T : TypeTag](value: T) extends Result[T]
sealed abstract class Computation[O : TypeTag] extends Result[O]

object Computation {
  def apply[I,O : TypeTag](f: Fun1[I,O], in: Result[I]): Computation1[I,O] = Computation1(f, in)
  def apply[I1,I2,O : TypeTag](f: Fun2[I1,I2,O], in1: Result[I1], in2: Result[I2]): Computation2[I1,I2,O] = Computation2(f, in1, in2)
}
final case class Computation1[I,O : TypeTag](fun1: Fun1[I,O], in: Result[I]) extends Result[O]
final case class Computation2[I1,I2,O : TypeTag](fun1: Fun2[I1,I2,O], in: Result[I1], in2: Result[I2]) extends Result[O]

object TypeAlias {
  type TT = Type

  final case class Val[+T](v: T) extends AnyVal
  type Top = Val[Any]
}

import TypeAlias._

sealed abstract class ResultF[F] {
  def typ: TT
}
case class InputF[F](name: String, typ: TT) extends ResultF[F]
case class CstF[F](value: Top, typ: TT) extends ResultF[F]
final case class ComputationF[F](fun: Fun[_],args: List[F], typ: TT) extends ResultF[F]

//case class Computation1F[I,O, F](fun1: Fun1[I,O], in: F) extends ResultF[O, F]
//case class Computation2F[I1, I2, O, F](fun1: Fun2[I1,I2,O], in1: F, in2: F) extends ResultF[O, F]




object Program extends App {

  import scalaz.{Divide => _, _}, Scalaz._
  implicit def traverse: Traverse[ResultF] = new Traverse[ResultF] {
    override def traverseImpl[G[_], A, B](fa: ResultF[A])(f: (A) => G[B])(implicit G: Applicative[G]) = fa match {
      case InputF(name, typ) => G.point(InputF(name, typ))
      case CstF(value, typ) => G.point(CstF(value, typ))
      case ComputationF(fun, args, typ) =>
        // split in multiple steps to help reason on types
        val tmp: List[G[B]] = args.map(f)
        def seqA(l: List[G[B]]): G[List[B]] = l match {
          case Nil => G.point(Nil)
          case x :: xs =>
            // Note: equivalent to (x |@| seqA(xs)) {_ :: _}
            // but without intermediate object creation
            ^(x, seqA(xs))(_ :: _)
        }
        val tmp2: G[List[B]] = seqA(tmp)
        val tmp3 = ^^(G.point(fun), tmp2, G.point(typ))(ComputationF(_, _, _): ResultF[B])
        tmp3
    }
  }

  val coalgebra: Coalgebra[ResultF, Result[_]] = {
    case x @ Input(name) => InputF(name, x.typ)
    case x @ Cst(value) => CstF(Val(value), x.typ)
    case x @ Computation1(fun, arg) => ComputationF(fun, List(arg), x.typ)
    case x @ Computation2(fun, arg1, arg2) => ComputationF(fun, List(arg1, arg2), x.typ)
  }

  object dsl {
    implicit class Fun2Ops[I1, I2, O : TypeTag](f: Fun2[I1, I2, O]) {
      def apply(i1: Result[I1], i2: Result[I2]): Computation2[I1,I2,O] =
        Computation(f, i1, i2)
    }

    implicit class InputHelper(val sc: StringContext) extends AnyVal {
      def d(args: Any*): Input[Double] = Input[Double](sc.s(args: _*))
    }
  }

  import dsl._

  val x = Input[Double]("x")
  val y = Input[Double]("y")
  val z = Input[Double]("z")

  val tmp = Computation(Min, x, y)
  val prg: Result[Boolean] = Computation(LEQ, tmp, z)

  val prg2: Result[Boolean] = LEQ(Min(d"x", d"y"), d"z")
  assert(prg == prg2)

  val pprint: Algebra[ResultF, String] = {
    case InputF(v, _)      => v
    case CstF(v, _) => v.toString
    case ComputationF(f, args, _) => f.name + args.mkString("(", ",", ")")
  }

  def eval(inputs: Map[String, Any]): Algebra[ResultF, Top] = {
    case InputF(v, _) => Val(inputs(v))
    case CstF(v, _) => v
    case ComputationF(f, args, _) => Val(f.compute(args.map(_.v)))
  }
  val rec = Recursive.fromCoalgebra(coalgebra)

  val inputs = Map("x" -> 1.0, "y" -> 2.0, "z" -> 5.0)

  println(rec.cata(prg)(pprint))
  println(rec.cata(prg)(eval(inputs)))

  println(prg2)

}
