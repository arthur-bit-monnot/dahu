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
      case fa: Total[A] => Total.functor.map(fa)(f)
      case x @ Partial(value, condition, typ) =>
        Partial(f(value), f(condition), typ)
    }
  }
}

/** Pure expressions that always yield value if they are fed with pure expressions.
  *
  * A Fix[Pure] can always be evaluated to its value.
  * */
sealed trait Total[F] extends ExprF[F]
object Total {
  implicit val functor: Functor[Total] = new Functor[Total] {
    override def map[A, B](fa: Total[A])(f: A => B): Total[B] = fa match {
      case x @ InputF(_, _) => x
      case x @ CstF(_, _)   => x
      case x @ ComputationF(fun, args, typ) =>
        ComputationF(fun, args.map(f), typ)
      case x @ ProductF(members, typ) => ProductF(members.map(f), typ)
    }
  }
}

/** An (unset) input to the problem.
  * Essentially a decision variable in CSP jargon. */
case class InputF[F](name: String, typ: Type) extends Total[F] {
  override def toString: String = s"$name"
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
  override def toString: String = s"λ: $fun(${args.mkString(", ")})"
}

final case class ProductF[F](members: Seq[F], typ: Type) extends Total[F] {
  override def toString: String = members.mkString("(", ", ", ")")
}

/** A partial expression that only produces a value if its condition evaluates to True. */
final case class Partial[F](value: F, condition: F, typ: Type) extends ExprF[F] {
  override def toString: String = s"$value? (constraint: $condition)"
}
