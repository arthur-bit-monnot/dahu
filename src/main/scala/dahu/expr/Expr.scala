package dahu.expr

import scala.reflect.runtime.universe.{typeOf, Type, TypeTag}

sealed abstract class Expr[+T: TypeTag] {
  final val typ = typeOf[T]
}
final case class Input[T: TypeTag](name: String) extends Expr[T] {
  def bind(value: T): Bind[T] = Bind(this, value)
}

final case class Cst[T: TypeTag](value: T)    extends Expr[T]
sealed abstract class Computation[O: TypeTag] extends Expr[O]

final case class Bind[T](variable: Input[T], value: T)

object Computation {
  def apply[I, O: TypeTag](f: Fun1[I, O], in: Expr[I]): Computation1[I, O] =
    Computation1(f, in)

  def apply[I1, I2, O: TypeTag](f: Fun2[I1, I2, O],
                                in1: Expr[I1],
                                in2: Expr[I2]): Computation2[I1, I2, O] =
    Computation2(f, in1, in2)

  def apply[I1, I2, I3, O: TypeTag](f: Fun3[I1, I2, I3, O],
                                    in1: Expr[I1],
                                    in2: Expr[I2],
                                    in3: Expr[I3]): Computation3[I1, I2, I3, O] =
    Computation3(f, in1, in2, in3)
}
final case class Computation1[I, O: TypeTag](fun1: Fun1[I, O], in: Expr[I]) extends Expr[O]

final case class Computation2[I1, I2, O: TypeTag](fun1: Fun2[I1, I2, O],
                                                  in: Expr[I1],
                                                  in2: Expr[I2])
    extends Expr[O]

final case class Computation3[I1, I2, I3, O: TypeTag](f: Fun3[I1, I2, I3, O],
                                                      in: Expr[I1],
                                                      in2: Expr[I2],
                                                      in3: Expr[I3])
    extends Expr[O]
