package dahu.model.ir

import cats.Functor
import dahu.model.functions.Fun
import dahu.model.input.Ident
import dahu.model.types.{ProductTag, Tag, Type, Value}

import scala.language.implicitConversions

sealed abstract class ExprF[F] {
  def typ: Type
}

sealed trait TotalOrOptionalF[F] { self: ExprF[F] =>
  def typ: Type
}
sealed trait TotalOrPartialF[F] { self: ExprF[F] =>
  def typ: Type
}
object TotalOrOptionalF {
  implicit val functor: Functor[TotalOrOptionalF] = new Functor[TotalOrOptionalF] {
    override def map[A, B](fa: TotalOrOptionalF[A])(f: A => B): TotalOrOptionalF[B] = fa match {
      case fa: Total[A] => Total.functor.map(fa)(f)
      case OptionalF(value, present, typ) =>
        OptionalF(f(value), f(present), typ)
    }
  }
}

object ExprF {
  implicit val functor: Functor[ExprF] = new Functor[ExprF] {
    override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case fa: Total[A] => Total.functor.map(fa)(f)
      case Partial(value, condition, typ) =>
        Partial(f(value), f(condition), typ)
      case OptionalF(value, present, typ) =>
        OptionalF(f(value), f(present), typ)
    }
  }
}

/** Pure expressions that always yield value if they are fed with pure expressions.
  *
  * A Fix[Pure] can always be evaluated to its value.
  * */
sealed trait Total[F] extends ExprF[F] with TotalOrOptionalF[F] with TotalOrPartialF[F]
object Total {
  implicit val functor: Functor[Total] = new Functor[Total] {
    override def map[A, B](fa: Total[A])(f: A => B): Total[B] = fa match {
      case x @ InputF(_, _)                 => x
      case x @ CstF(_, _)                   => x
      case ComputationF(fun, args, typ)     => ComputationF(fun, args.map(f), typ)
      case ProductF(members, typ)           => ProductF(members.map(f), typ)
      case ITEF(cond, onTrue, onFalse, typ) => ITEF(f(cond), f(onTrue), f(onFalse), typ)
      case PresentF(v)                      => PresentF(f(v))
      case ValidF(v)                        => ValidF(f(v))
    }
  }
}

/** An (unset) input to the problem.
  * Essentially a decision variable in CSP jargon. */
case class InputF[F](id: Ident, typ: Type) extends Total[F] {
  override def toString: String = s"$id"
}
object InputF {

  /** The type parameter of InputF is does not play any role beside allowing recursion scheme.
    * This implicit conversion, allows usin it interchangeably without creating new objects. or casting manually*/
  implicit def typeParamConversion[F, G](fa: InputF[F]): InputF[G] = fa.asInstanceOf[InputF[G]]
}

case class CstF[F](value: Value, typ: Type) extends Total[F] {
  override def toString: String = value.toString
}
object CstF {

  /** Leaf node, with  artificial type parameters, allow implicit conversion as for InputF. */
  implicit def typeParamConversion[F, G](fa: CstF[F]): CstF[G] = fa.asInstanceOf[CstF[G]]
}

final case class ComputationF[F](fun: Fun[_], args: Seq[F], typ: Type) extends Total[F] {
  override def toString: String = s"$fun(${args.mkString(", ")})"
}

final case class ProductF[F](members: Seq[F], typ: ProductTag[Any]) extends Total[F] {
  override def toString: String = members.mkString("(", ", ", ")")
}

final case class ITEF[F](cond: F, onTrue: F, onFalse: F, typ: Type) extends Total[F] {
  override def toString: String = s"ite($cond, $onTrue, $onFalse)"
}

final case class PresentF[F](optional: F) extends Total[F] {
  override def typ: Type = Tag.ofBoolean

  override def toString: String = s"present($optional)"
}

final case class ValidF[F](partial: F) extends Total[F] {
  override def typ: Type = Tag.ofBoolean

  override def toString: String = s"valid($partial)"
}

/** An Optional expression, that evaluates to Some(value) if present == true and to None otherwise. */
final case class OptionalF[F](value: F, present: F, typ: Type)
    extends ExprF[F]
    with TotalOrOptionalF[F] {
  override def toString: String = s"$value? (presence: $present)"
}

/** A partial expression that only produces a value if its condition evaluates to True. */
final case class Partial[F](value: F, condition: F, typ: Type)
    extends ExprF[F]
    with TotalOrPartialF[F] {
  override def toString: String = s"$value? (constraint: $condition)"
}
