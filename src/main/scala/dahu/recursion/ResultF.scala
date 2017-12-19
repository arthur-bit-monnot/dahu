package dahu.recursion

import dahu.expr.Fun
import dahu.expr.labels.Labels.Value

object TypeAlias {
  sealed trait ValueTag
  type Type = scala.reflect.runtime.universe.Type
}

import TypeAlias._

sealed abstract class ResultF[F] {
  def typ: Type
}

case class InputF[F](name: String, typ: Type) extends ResultF[F] {
  override def toString: String = s"?$name"
}
case class CstF[F](value: Value, typ: Type) extends ResultF[F] {
  override def toString: String = value.toString
}
final case class ComputationF[F](fun: Fun[_], args: List[F], typ: Type) extends ResultF[F] {
  override def toString: String = s"λ: $fun(${args.mkString(", ")})"
}
