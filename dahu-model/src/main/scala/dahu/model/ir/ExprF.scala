package dahu.model.ir

import cats.Functor
import dahu.model.functions.Fun
import dahu.model.types.{Type, Value}

import scala.language.implicitConversions

sealed abstract class ExprF[F] {
  def typ: Type
}

object ExprF {
  implicit val functor: Functor[ExprF] = new Functor[ExprF] {
    override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case x @ InputF(_, _) => x
      case x @ CstF(_, _)   => x
      case x @ ComputationF(fun, args, typ) =>
        ComputationF(fun, args.map(f), typ)
      case x @ SubjectToF(value, condition, typ) =>
        SubjectToF(f(value), f(condition), typ)
      case x @ ProductF(members, typ) => ProductF(members.map(f), typ)
    }
  }
}

case class InputF[F](name: String, typ: Type) extends ExprF[F] {
  override def toString: String = s"$name"
}
object InputF {
  implicit def typeParamConversion[F, G](fa: InputF[F]): InputF[G] = fa.asInstanceOf[InputF[G]]
}

case class CstF[F](value: Value, typ: Type) extends ExprF[F] {
  override def toString: String = value.toString
}
object CstF {
  implicit def typeParamConversion[F, G](fa: CstF[F]): CstF[G] = fa.asInstanceOf[CstF[G]]
}

final case class ComputationF[F](fun: Fun[_], args: Seq[F], typ: Type) extends ExprF[F] {
  override def toString: String = s"λ: $fun(${args.mkString(", ")})"
}

final case class SubjectToF[F](value: F, condition: F, typ: Type) extends ExprF[F] {
  override def toString: String = s"$value?"
}

final case class ProductF[F](members: Seq[F], typ: Type) extends ExprF[F] {
  override def toString: String = members.mkString("(", ", ", ")")
}
