package dahu.recursion


import dahu.expr.Fun
import matryoshka.instances.fixedpoint.Cofree

import scala.reflect.runtime.universe.{Type, TypeTag, typeOf}

object TypeAlias {
  type TT = Type
  type Top = Val[Any]
}

import TypeAlias._

final case class Val[+T](v: T) extends AnyVal

sealed abstract class ResultF[F] {
  def typ: TT
}

case class InputF[F](name: String, typ: TT) extends ResultF[F]
case class CstF[F](value: Top, typ: TT) extends ResultF[F]
final case class ComputationF[F](fun: Fun[_],args: List[F], typ: TT) extends ResultF[F]


object Test {

  import matryoshka._
  import matryoshka.data._

}