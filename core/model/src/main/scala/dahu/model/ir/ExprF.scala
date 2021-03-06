package dahu.model.ir

import dahu.graphs.TreeNode
import dahu.utils._
import dahu.model.functions.{Fun, FunAny}
import dahu.model.input.{Ident, Lambda, TypedIdent}
import dahu.model.products.{ProductTag, ProductTagAny}
import dahu.model.types.{LambdaTag, SequenceTag, SequenceTagAny, Tag, TagIsoInt, Type, Value}
import shapeless.=:!=

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime
import scala.collection.immutable.Iterable

sealed trait ExprF[@sp(Int) F] {

//  if(typ == Tag.unsafe.ofAny && !this.isInstanceOf[LambdaParamF[_]])
//    System.err.println("ANY: " + this)

  def typ: Type

  override final lazy val hashCode: Int = ExprF.hash(this)
}
object ExprF extends LowPriorityExprF {
  implicit val functor: SFunctor[ExprF] = new SFunctor[ExprF] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: ExprF[A])(f: A => B): ExprF[B] =
      fa match {
        case fa: Total[A]               => Total.functor.smap(fa)(f)
        case DynCollectorF(a, b, c)     => DynCollectorF(a, b, c)
        case DynInputF(a)               => DynInputF(a)
        case ApplyF(lambda, param, typ) => ApplyF(f(lambda), f(param), typ)
      }
  }

  def hash[@sp(Int) A](exprF: ExprF[A]): Int = exprF match {
    case x: ComputationF[A]  => ScalaRunTime._hashCode(x)
    case x: InputF[A]        => ScalaRunTime._hashCode(x)
    case x: CstF[A]          => ScalaRunTime._hashCode(x)
    case x: ITEF[A]          => ScalaRunTime._hashCode(x)
    case x: ProductF[A]      => ScalaRunTime._hashCode(x)
    case x: DynInputF[A]     => ScalaRunTime._hashCode(x)
    case x: DynCollectorF[A] => ScalaRunTime._hashCode(x)
    case x: LambdaF[A]       => ScalaRunTime._hashCode(x)
    case x: ApplyF[A]        => ScalaRunTime._hashCode(x)
    case x: LambdaParamF[A]  => ScalaRunTime._hashCode(x)
    case x: SequenceF[A]     => ScalaRunTime._hashCode(x)
    case x: NoopF[A]         => ScalaRunTime._hashCode(x)
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
      case x: Total[A]              => Total.treeNodeInstance.children(x)
      case x: DynInputF[A]          => Iterable.empty
      case x: DynCollectorF[A]      => Iterable.empty
      case ApplyF(lambda, param, _) => Iterable(lambda, param)
    }
    override def foreachChild[A](fa: ExprF[A])(f: A => Unit): Unit = fa match {
      case x: Total[A]              => Total.treeNodeInstance.foreachChild(x)(f)
      case x: DynCollectorF[A]      =>
      case x: DynInputF[A]          =>
      case ApplyF(lambda, param, _) => f(lambda); f(param)
    }
  }
}

sealed trait StaticF[@sp(Int) F] extends ExprF[F]

/** Pure expressions that always yield value if they are fed with pure expressions.
  *
  * A Fix[Pure] can always be evaluated to its value.
  * */
sealed trait Total[@sp(Int) F] extends StaticF[F] // StaticF without lambda application
object Total {
  implicit val functor: SFunctor[Total] = new SFunctor[Total] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: Total[A])(f: A => B): Total[B] =
      fa match {
        case x: InputF[A]                     => x
        case x: CstF[A]                       => x
        case ComputationF(fun, args, typ)     => ComputationF(fun, args.map(f), typ)
        case ProductF(members, typ)           => ProductF(members.map(f), typ)
        case ITEF(cond, onTrue, onFalse, typ) => ITEF(f(cond), f(onTrue), f(onFalse), typ)
        case SequenceF(members, typ)          => SequenceF(members.map(f), typ)
        case LambdaF(in, tree, id, typ)       => LambdaF(f(in), f(tree), id, typ)
        case LambdaParamF(l, t)               => LambdaParamF(l, t)
        case NoopF(e, t)                      => NoopF(f(e), t)
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
      case LambdaF(in, tree, _, _)  => Iterable(in, tree)
      case NoopF(e, _)              => Iterable(e)
      case _: LambdaParamF[A]       => Iterable.empty
    }
    override def foreachChild[A](fa: Total[A])(f: A => Unit): Unit = fa match {
      case ComputationF(_, args, _) => args.foreach(f)
      case _: CstF[A]               =>
      case _: InputF[A]             =>
      case ITEF(c, t, onFalse, _)   => f(c); f(t); f(onFalse)
      case ProductF(as, _)          => as.foreach(f)
      case SequenceF(as, _)         => as.foreach(f)
      case LambdaF(in, tree, _, _)  => f(in); f(tree)
      case NoopF(e, _)              => f(e)
      case _: LambdaParamF[A]       =>
    }
  }
}

/** An (unset) input to the problem.
  * Essentially a decision variable in CSP jargon. */
final case class InputF[@sp(Int) F](id: TypedIdent, typ: Type) extends Total[F] {
  // todo: check the useful of the typ field that should be redundant with the TypedIdent.
  //       the second assert also looks outdated
  require(typ != null)
  require(typ == id.typ || id.typ.isInstanceOf[TagIsoInt[_]] && typ == Tag.ofInt) // todo: this is a sanity check for current assumption but might not hold for valid future uses
  override def toString: String = s"$id"
}
object InputF {

  def apply[F](id: Ident, typ: Type): InputF[F] = InputF(TypedIdent(id, typ), typ)
  def apply[F](id: TypedIdent): InputF[F] = InputF(id, id.typ)

  /** The type parameter of InputF does not play any role beside allowing recursion scheme.
    * This implicit conversion, allows using it interchangeably without creating new objects. or casting manually*/
  implicit def typeParamConversion[F, G](fa: InputF[F]): InputF[G] = fa.asInstanceOf[InputF[G]]
}

final case class CstF[@sp(Int) F](value: Value, typ: Type) extends Total[F] {
  assert(!typ.isBoolean || value == 0 || value == 1)
//  if(value == 0 && typ == Tag.unsafe.ofAny)
//    System.err.println(this.toString + " " + typ)
//  assert(!typ.isInstanceOf[TagIsoInt[_]])
  override def toString: String = value.toString
}
object CstF {

  def of[F](value: Any, typ: Type): CstF[F] = new CstF(Value(value), typ)

  /** Leaf node, with  artificial type parameters, allow implicit conversion as for InputF. */
  implicit def typeParamConversion[F, G](fa: CstF[F]): CstF[G] = fa.asInstanceOf[CstF[G]]
}

final case class ComputationF[@sp(Int) F](fun: FunAny, args: Vec[F], typ: Type) extends Total[F] {
  require(typ != null)
  override def toString: String = {
//    if(fun.name == "box" || fun.name == "unbox") args(0).toString
//    else
    s"$fun(${args.mkString(", ")})"
  }
}
object ComputationF {
  def apply[F: ClassTag](fun: FunAny, args: Seq[F], tpe: Type): ComputationF[F] =
    new ComputationF(fun, Vec.fromArray(args.toArray), tpe)

  def apply[F: ClassTag](fun: FunAny, args: F*): ComputationF[F] =
    new ComputationF(fun, Vec.fromArray(args.toArray), fun.outType)

  def apply[F: ClassTag](fun: FunAny, args: Vec[F]): ComputationF[F] =
    new ComputationF(fun, args, fun.outType)
}

final case class SequenceF[@sp(Int) F](members: Vec[F], typ: SequenceTagAny) extends Total[F] {
  override def toString: String = members.mkString("[", ", ", "]")
}
object SequenceF {
  def apply[F: ClassTag](args: Seq[F], tpe: SequenceTagAny): SequenceF[F] =
    new SequenceF[F](Vec.fromSeq(args), tpe)
}

final case class ProductF[@sp(Int) F](members: Vec[F], typ: ProductTagAny) extends Total[F] {
  override def toString: String =
    typ.toString
      .replaceFirst("\\[cats\\.Id\\]", "")
      .reverse
      .takeWhile(_ != '.')
      .reverse + members
      .mkString("(", ", ", ")")
}
object ProductF {
  def apply[F: ClassTag](args: Seq[F], tpe: ProductTagAny): ProductF[F] =
    new ProductF[F](Vec.fromSeq(args), tpe)
}

final case class ITEF[@sp(Int) F](cond: F, onTrue: F, onFalse: F, typ: Type) extends Total[F] {
  override def toString: String = s"ite($cond, $onTrue, $onFalse)"
}

sealed trait DynExprF[F] extends ExprF[F]

final case class DynInputF[F](id: TypedIdent) extends DynExprF[F] {
  override def typ: Type = id.typ
}

final case class DynCollectorF[F](collectedType: Type, filter: Option[Type => Boolean], typ: Type)
    extends DynExprF[F]

/**
  * Lambda is composed of an AST `tree` and a variable `in`.
  * `in` appears in the AST, and should be replaced with the parameter when applying the lambda.
  */
final case class LambdaF[F](in: F, tree: F, id: Lambda.LambdaIdent, tpe: LambdaTag[_, _])
    extends Total[F] {
  // strangely, declaring this in the parameters results in AbstractMethodError when called
  override def typ: Type = tpe

  override def toString: String = s"λ$in.$tree"
}

final case class ApplyF[F](lambda: F, param: F, typ: Type) extends StaticF[F] {
  override def toString: String = s"($lambda $param)"
}

final case class LambdaParamF[F](id: Lambda.LambdaIdent, typ: Type) extends Total[F] {
  override def toString: String = id.toString
}

final case class NoopF[F](e: F, typ: Type) extends Total[F] {
  override def toString: String = s"noop($e)"
}
