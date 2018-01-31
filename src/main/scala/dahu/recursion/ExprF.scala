package dahu.recursion

import cats.Functor
import dahu.arrows.{==>, OpaqueIntSubset}
import dahu.ast.{IndexLabelImpl, SubIndexLabelImpl}
import dahu.expr.{Fun, Type}
import dahu.expr.labels.Labels.Value

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
      case x @ ProductF(members, typ) => ProductF(members.map(f), typ)
    }
  }
}

case class InputF[F](name: String, typ: Type) extends ExprF[F] {
  override def toString: String = s"?$name"
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

final case class ProductF[F](members: Seq[F], typ: Type) extends ExprF[F] {
  override def toString: String = members.mkString("(", ", ", ")")
}


/**
  * Abstract Syntax Directed Acyclic Graph.
  * Provides:
  *
  * Two opaque type alias of Int:
  *  - EId: represents the set of identifiers for all expressions
  *  - VarId: a subset of EId that represents decision variables
  *
  * A partial function from T => EId
  * A coalgebra EId => ExprF[EId]
  */
trait ASDAG[T] {

  /** Opaque type representing the IDs of the expression in this table. */
  val EId: IndexLabelImpl = new IndexLabelImpl {
    type T = Int
    override def fromInt(s: Int): T               = s
    override def toInt(lbl: T): Int               = lbl
    override def fromIntF[F[_]](fs: F[Int]): F[T] = fs
    override def toIntF[F[_]](fs: F[T]): F[Int]   = fs
  }
  type EId = EId.T

  val VarId: SubIndexLabelImpl[EId] = new SubIndexLabelImpl[EId] {
    type T = EId

    override def fromInt(s: Int): EId               = EId.fromInt(s)
    override def toInt(lbl: T): Int                 = EId.toInt(lbl)
    override def toIntF[F[_]](fs: F[EId]): F[Int]   = EId.toIntF(fs)
    override def fromIntF[F[_]](fs: F[Int]): F[EId] = EId.fromIntF(fs)

    override def wrap(s: EId): T                 = s
    override def unwrap(lbl: T): EId             = lbl
    override def subst[F[_]](fs: F[EId]): F[T]   = fs
    override def unsubst[F[_]](fs: F[T]): F[EId] = fs
  }
  type VarId = VarId.T

  type Expr     = ExprF[EId]
  type Variable = InputF[EId]

  def root: EId
  def coalgebra: EId ==> Expr // equivalent to Coalgebra[ExprF, EId]

  def compiledForm: T ==> Option[EId]

  def variableCoalgebra: VarId ==> Variable = id => coalgebra(VarId.unwrap(id)).asInstanceOf[Variable]

  def ids: OpaqueIntSubset[EId]
  def variableIds: OpaqueIntSubset[VarId]

}
