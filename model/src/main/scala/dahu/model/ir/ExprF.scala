package dahu.model.ir

import dahu.{ImmutableArray, SFunctor}
import dahu.model.functions.Fun
import dahu.model.input.Ident
import dahu.model.types.{ProductTag, Tag, Type, Value}
import scala.{specialized => sp}

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime

sealed trait ExprF[@sp(Int) F] {
  def typ: Type

  override final lazy val hashCode: Int = ExprF.hash(this)
}

sealed trait TotalOrOptionalF[@sp(Int) F] { self: ExprF[F] =>
  def typ: Type
}
sealed trait TotalOrPartialF[@sp(Int) F] { self: ExprF[F] =>
  def typ: Type
}
object TotalOrOptionalF {
  implicit val functor: SFunctor[TotalOrOptionalF] = new SFunctor[TotalOrOptionalF] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: TotalOrOptionalF[A])(f: A => B): TotalOrOptionalF[B] = fa match {
      case fa: Total[A] => Total.functor.smap(fa)(f)
      case OptionalF(value, present, typ) =>
        OptionalF(f(value), f(present), typ)
    }
  }
}

object ExprF {
  implicit val functor: SFunctor[ExprF] = new SFunctor[ExprF] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case fa: Total[A] => Total.functor.smap(fa)(f)
      case Partial(value, condition, typ) =>
        Partial(f(value), f(condition), typ)
      case OptionalF(value, present, typ) =>
        OptionalF(f(value), f(present), typ)
      case PresentF(v) => PresentF(f(v))
      case ValidF(v)   => ValidF(f(v))
    }
  }

  def hash[@sp(Int) A](exprF: ExprF[A]): Int = exprF match {
    case x: ComputationF[A] => ScalaRunTime._hashCode(x)
    case x: InputF[A]       => ScalaRunTime._hashCode(x)
    case x: CstF[A]         => ScalaRunTime._hashCode(x)
    case x: Partial[A]      => ScalaRunTime._hashCode(x)
    case x: OptionalF[A]    => ScalaRunTime._hashCode(x)
    case x: PresentF[A]     => ScalaRunTime._hashCode(x)
    case x: ValidF[A]       => ScalaRunTime._hashCode(x)
    case x: ITEF[A]         => ScalaRunTime._hashCode(x)
    case x: ProductF[A]     => ScalaRunTime._hashCode(x)
  }
}

/** Pure expressions that always yield value if they are fed with pure expressions.
  *
  * A Fix[Pure] can always be evaluated to its value.
  * */
sealed trait Total[@sp(Int) F]
    extends ExprF[F]
    with TotalOrOptionalF[F]
    with TotalOrPartialF[F]
object Total {
  implicit val functor: SFunctor[Total] = new SFunctor[Total] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: Total[A])(f: A => B): Total[B] =
      fa match {
        case x @ InputF(_, _)                 => x
        case x @ CstF(_, _)                   => x
        case ComputationF(fun, args, typ)     => new ComputationF(fun,  implicitly[SFunctor[ImmutableArray]].smap(args)(f), typ)
        case ProductF(members, typ)           => ProductF(members.map(f), typ)
        case ITEF(cond, onTrue, onFalse, typ) => ITEF(f(cond), f(onTrue), f(onFalse), typ)
      }
  }
  trait FunctorSpec[F[_]] {
    def map[A, B: ClassTag](fa: F[A])(f: A => B): F[B]
  }
}

/** An (unset) input to the problem.
  * Essentially a decision variable in CSP jargon. */
case class InputF[@sp(Int) F](id: Ident, typ: Type) extends Total[F] {
  override def toString: String = s"$id"
}
object InputF {

  /** The type parameter of InputF is does not play any role beside allowing recursion scheme.
    * This implicit conversion, allows usin it interchangeably without creating new objects. or casting manually*/
  implicit def typeParamConversion[F, G](fa: InputF[F]): InputF[G] = fa.asInstanceOf[InputF[G]]
}

case class CstF[@sp(Int) F](value: Value, typ: Type) extends Total[F] {
  override def toString: String = value.toString
}
object CstF {

  /** Leaf node, with  artificial type parameters, allow implicit conversion as for InputF. */
  implicit def typeParamConversion[F, G](fa: CstF[F]): CstF[G] = fa.asInstanceOf[CstF[G]]
}

final case class ComputationF[@sp(Int) F](fun: Fun[_], args: ImmutableArray[F], typ: Type)
    extends Total[F] {
  override def toString: String = s"$fun(${args.mkString(", ")})"
}
object ComputationF {
  def apply[F : ClassTag](fun: Fun[_], args: Seq[F], tpe: Type): ComputationF[F] =
    new ComputationF(fun, ImmutableArray.fromArray(args.toArray), tpe)

}

final case class ProductF[@sp(Int) F](members: ImmutableArray[F], typ: ProductTag[Any])
    extends Total[F] {
  override def toString: String = members.mkString("(", ", ", ")")
}
object ProductF {
  def apply[F: ClassTag](args: Seq[F], tpe: ProductTag[Any]): ProductF[F] =
    new ProductF[F](ImmutableArray.fromArray(args.toArray), tpe)
}

final case class ITEF[@sp(Int) F](cond: F, onTrue: F, onFalse: F, typ: Type)
    extends Total[F] {
  override def toString: String = s"ite($cond, $onTrue, $onFalse)"
}

final case class PresentF[F](optional: F) extends ExprF[F] {
  override def typ: Type = Tag.ofBoolean

  override def toString: String = s"present($optional)"
}

final case class ValidF[@sp(Int) F](partial: F) extends ExprF[F] {
  override def typ: Type = Tag.ofBoolean

  override def toString: String = s"valid($partial)"
}

/** An Optional expression, that evaluates to Some(value) if present == true and to None otherwise. */
final case class OptionalF[@sp(Int) F](value: F, present: F, typ: Type)
    extends ExprF[F]
    with TotalOrOptionalF[F] {
  override def toString: String = s"$value? (presence: $present)"
}

/** A partial expression that only produces a value if its condition evaluates to True. */
final case class Partial[@sp(Int) F](value: F, condition: F, typ: Type)
    extends ExprF[F]
    with TotalOrPartialF[F] {
  override def toString: String = s"$value? (constraint: $condition)"
}
