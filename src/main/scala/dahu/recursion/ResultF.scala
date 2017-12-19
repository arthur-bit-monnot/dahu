package dahu.recursion

import dahu.expr.Fun

object TypeAlias {
  sealed trait ValueTag
  type Type = scala.reflect.runtime.universe.Type
  type Top  = Any
}

import TypeAlias._

sealed abstract class ResultF[F] {
  def typ: Type
}

case class InputF[F](name: String, typ: Type) extends ResultF[F] {
  override def toString: String = s"?$name"
}
case class CstF[F](value: Top, typ: Type) extends ResultF[F] {
  override def toString: String = value.toString
}
final case class ComputationF[F](fun: Fun[_], args: List[F], typ: Type) extends ResultF[F] {
  override def toString: String = s"λ: $fun(${args.mkString(", ")})"
}
