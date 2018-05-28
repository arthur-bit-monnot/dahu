package dahu.model.ir

import dahu.graphs.TreeNode
import dahu.utils._
import dahu.model.functions.Fun
import dahu.model.input.Ident
import dahu.model.math.Monoid
import dahu.model.types.{LambdaTag, ProductTag, SequenceTag, Tag, Type, Value}
import shapeless.=:!=

import scala.{specialized => sp}
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime
import scala.collection.immutable.Iterable

sealed trait ExprF[@sp(Int) F] {
  def typ: Type

  override final lazy val hashCode: Int = ExprF.hash(this)
}
object ExprF extends LowPriorityExprF {
  implicit val functor: SFunctor[ExprF] = new SFunctor[ExprF] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: ExprF[A])(f: A => B): ExprF[B] =
      fa match {
        case fa: Total[A] => Total.functor.smap(fa)(f)
        case Partial(value, condition, typ) =>
          Partial(f(value), f(condition), typ)
        case OptionalF(value, present, typ) =>
          OptionalF(f(value), f(present), typ)
        case PresentF(v)                 => PresentF(f(v))
        case ValidF(v)                   => ValidF(f(v))
        case DynamicF(fun, monoid, typ)  => DynamicF(f(fun), monoid, typ)
        case DynamicProviderF(e, p, typ) => DynamicProviderF(f(e), f(p), typ)
        case ApplyF(lambda, param, typ)  => ApplyF(f(lambda), f(param), typ)
        case LambdaF(in, tree, id, typ)  => LambdaF(f(in), f(tree), id, typ)
        case LambdaParamF(l, t)          => LambdaParamF(l, t)
      }
  }

  def hash[@sp(Int) A](exprF: ExprF[A]): Int = exprF match {
    case x: ComputationF[A]     => ScalaRunTime._hashCode(x)
    case x: InputF[A]           => ScalaRunTime._hashCode(x)
    case x: CstF[A]             => ScalaRunTime._hashCode(x)
    case x: Partial[A]          => ScalaRunTime._hashCode(x)
    case x: OptionalF[A]        => ScalaRunTime._hashCode(x)
    case x: PresentF[A]         => ScalaRunTime._hashCode(x)
    case x: ValidF[A]           => ScalaRunTime._hashCode(x)
    case x: ITEF[A]             => ScalaRunTime._hashCode(x)
    case x: ProductF[A]         => ScalaRunTime._hashCode(x)
    case x: DynamicF[A]         => ScalaRunTime._hashCode(x)
    case x: DynamicProviderF[A] => ScalaRunTime._hashCode(x)
    case x: LambdaF[A]          => ScalaRunTime._hashCode(x)
    case x: ApplyF[A]           => ScalaRunTime._hashCode(x)
    case x: LambdaParamF[A]     => ScalaRunTime._hashCode(x)
    case x: SequenceF[A]        => ScalaRunTime._hashCode(x)
  }
}
trait LowPriorityExprF {
  // note this is safe to do as the functor instance never changes the wrapping type
  implicit def subFunctorInstance[F[_]](implicit ev: F[Any] <:< ExprF[Any],
                                        ev2: F[Any] =:!= ExprF[Any],
                                        ev3: F[Any] =:!= Total[Any]): SFunctor[F] =
    ExprF.functor.asInstanceOf[SFunctor[F]]

  implicit val treeNodeInstance: TreeNode[ExprF] = new TreeNode[ExprF] {
    override def children[A](fa: ExprF[A]): Iterable[A] = fa match {
      case x: Total[A]                      => Total.treeNodeInstance.children(x)
      case Partial(value, condition, typ)   => Iterable(value, condition)
      case OptionalF(value, present, typ)   => Iterable(value, present)
      case PresentF(v)                      => Iterable(v)
      case ValidF(v)                        => Iterable(v)
      case DynamicF(f, _, _)                => Iterable(f)
      case DynamicProviderF(e, provided, _) => Iterable(e, provided)
      case ApplyF(lambda, param, _)         => Iterable(lambda, param)
      case LambdaF(in, tree, _, _)          => Iterable(in, tree)
      case _: LambdaParamF[A]               => Iterable.empty
    }
  }
}

sealed trait NoProviderF[@sp(Int) F] extends ExprF[F]
sealed trait StaticF[@sp(Int) F] extends NoProviderF[F]
sealed trait NoApplyF[@sp(Int) F] extends StaticF[F]
sealed trait NoLambdas[@sp(Int) F] extends NoApplyF[F]

/** Pure expressions that always yield value if they are fed with pure expressions.
  *
  * A Fix[Pure] can always be evaluated to its value.
  * */
sealed trait Total[@sp(Int) F] extends NoLambdas[F]
object Total {
  implicit val functor: SFunctor[Total] = new SFunctor[Total] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: Total[A])(f: A => B): Total[B] =
      fa match {
        case x @ InputF(_, _)                 => x
        case x @ CstF(_, _)                   => x
        case ComputationF(fun, args, typ)     => ComputationF(fun, args.map(f), typ)
        case ProductF(members, typ)           => ProductF(members.map(f), typ)
        case ITEF(cond, onTrue, onFalse, typ) => ITEF(f(cond), f(onTrue), f(onFalse), typ)
        case SequenceF(members, typ)          => SequenceF(members.map(f), typ)
      }
  }

  implicit val treeNodeInstance: TreeNode[Total] = new TreeNode[Total] {
    override def children[A](fa: Total[A]): Iterable[A] = fa match {
      case ComputationF(_, args, _) => args.toIterable
      case _: CstF[A]               => Iterable.empty
      case _: InputF[A]             => Iterable.empty
      case ITEF(c, t, f, _)         => Iterable(c, t, f)
      case ProductF(as, _)          => as.toIterable
      case SequenceF(as, _)         => as.toIterable
    }
  }
}

/** An (unset) input to the problem.
  * Essentially a decision variable in CSP jargon. */
final case class InputF[@sp(Int) F](id: Ident, typ: Type) extends Total[F] {
  override def toString: String = s"$id"
}
object InputF {

  /** The type parameter of InputF does not play any role beside allowing recursion scheme.
    * This implicit conversion, allows using it interchangeably without creating new objects. or casting manually*/
  implicit def typeParamConversion[F, G](fa: InputF[F]): InputF[G] = fa.asInstanceOf[InputF[G]]
}

final case class CstF[@sp(Int) F](value: Value, typ: Type) extends Total[F] {
  override def toString: String = value.toString
}
object CstF {

  /** Leaf node, with  artificial type parameters, allow implicit conversion as for InputF. */
  implicit def typeParamConversion[F, G](fa: CstF[F]): CstF[G] = fa.asInstanceOf[CstF[G]]
}

final case class ComputationF[@sp(Int) F](fun: Fun[_], args: Vec[F], typ: Type) extends Total[F] {
  override def toString: String = {
    if(fun.name == "box" || fun.name == "unbox") args(0).toString
    else s"$fun(${args.mkString(", ")})"
  }
}
object ComputationF {
  def apply[F: ClassTag](fun: Fun[_], args: Seq[F], tpe: Type): ComputationF[F] =
    new ComputationF(fun, Vec.fromArray(args.toArray), tpe)

}

final case class SequenceF[@sp(Int) F](members: Vec[F], typ: SequenceTag[Any]) extends Total[F] {
  override def toString: String = members.mkString("[", ", ", "]")
}
object SequenceF {
  def apply[F: ClassTag](args: Seq[F], tpe: SequenceTag[Any]): SequenceF[F] =
    new SequenceF[F](Vec.fromSeq(args), tpe)
}

final case class ProductF[@sp(Int) F](members: Vec[F], typ: ProductTag[Any]) extends Total[F] {
  override def toString: String =
    typ.typ.toString
      .replaceFirst("\\[cats\\.Id\\]", "")
      .reverse
      .takeWhile(_ != '.')
      .reverse + members
      .mkString("(", ", ", ")")
}
object ProductF {
  def apply[F: ClassTag](args: Seq[F], tpe: ProductTag[Any]): ProductF[F] =
    new ProductF[F](Vec.fromSeq(args), tpe)
}

final case class ITEF[@sp(Int) F](cond: F, onTrue: F, onFalse: F, typ: Type) extends Total[F] {
  override def toString: String = s"ite($cond, $onTrue, $onFalse)"
}

final case class PresentF[F](optional: F) extends ExprF[F] with NoLambdas[F] {
  override def typ: Type = Tag[Boolean]

  override def toString: String = s"present($optional)"
}

final case class ValidF[@sp(Int) F](partial: F) extends NoLambdas[F] {
  override def typ: Type = Tag[Boolean]

  override def toString: String = s"valid($partial)"
}

/** An Optional expression, that evaluates to Some(value) if present == true and to None otherwise. */
final case class OptionalF[@sp(Int) F](value: F, present: F, typ: Type) extends NoLambdas[F] {
  override def toString: String = s"$value? (presence: $present)"
}

/** A partial expression that only produces a value if its condition evaluates to True. */
final case class Partial[@sp(Int) F](value: F, condition: F, typ: Type)
    extends ExprF[F]
    with NoLambdas[F] {
  override def toString: String = s"$value? (constraint: $condition)"
}

final case class DynamicF[@sp(Int) F](f: F, monoid: Monoid[_], typ: Type) extends NoProviderF[F]

final case class DynamicProviderF[@sp(Int) F](e: F, provided: F, typ: Type) extends ExprF[F]

/**
  * Lambda is composed of an AST `tree` and a variable `in`.
  * `in` appears in the AST, and should be replaced with the parameter when applying the lambda.
  */
final case class LambdaF[F](in: F, tree: F, id: Ident, tpe: LambdaTag[_, _]) extends NoApplyF[F] {
  // strangely, declaring this in the parameters results in AbstractMethodError when called
  override def typ: Type = tpe

  override def toString: String = s"λ$in.$tree"
}

final case class ApplyF[F](lambda: F, param: F, typ: Type) extends StaticF[F] {
  override def toString: String = s"($lambda $param)"
}

final case class LambdaParamF[F](id: Ident, typ: Type) extends NoApplyF[F] {
  override def toString: String = "???" + id.toString
}
