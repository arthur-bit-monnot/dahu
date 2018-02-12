package dahu.recursion

import cats.Functor
import dahu.arrows.{==>, OpaqueIntSubset}
import dahu.ast.{IndexLabelImpl, SubIndexLabelImpl}
import dahu.expr.{Fun, Type}
import dahu.expr.labels.Labels.Value

import scala.language.implicitConversions
import scala.reflect.ClassTag

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


object Types {

  abstract class IndexLabelImpl {
    type T
    def apply(s: Int): T = fromInt(s)

    def fromInt(s: Int): T
    def toInt(lbl: T): Int
    def fromIntF[F[_]](fs: F[Int]): F[T]
    def toIntF[F[_]](fs: F[T]): F[Int]
  }

  abstract class SubIndexLabelImpl[UB] extends IndexLabelImpl {
    def wrap(s: UB): T
    def unwrap(lbl: T): UB
    def subst[F[_]](fs: F[UB]): F[T]
    def unsubst[F[_]](fs: F[T]): F[UB]
  }

  val ExprId: IndexLabelImpl = new IndexLabelImpl {
    type T = Int
    override def fromInt(s: Int): T               = s
    override def toInt(lbl: T): Int               = lbl
    override def fromIntF[F[_]](fs: F[Int]): F[T] = fs
    override def toIntF[F[_]](fs: F[T]): F[Int]   = fs
  }
  type ExprId = ExprId.T

  /** Implicit conversion to Int, mainly to facilitate usage as index. */
  implicit def exprIdAsInt(i: ExprId): Int = ExprId.toInt(i)
  implicit class ExprIdOps(val i: ExprId) extends AnyVal {
    def value: Int = ExprId.toInt(i)
  }

  implicit val classTag: ClassTag[ExprId] = ExprId.fromIntF(implicitly[ClassTag[Int]])
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
  import Types._

  /** Opaque type representing the IDs of the expression in this table. */
  type VariableId = ExprId

  type Expr     = ExprF[ExprId]
  type Variable = InputF[ExprId]

  def root: ExprId
  def coalgebra: ExprId ==> Expr // equivalent to Coalgebra[ExprF, EId]

  def compiledForm: T ==> Option[ExprId]

  def variableCoalgebra: VariableId ==> Variable = id => coalgebra(id).asInstanceOf[Variable] // TODO: unsafe as VarID is just an alias for ExprId

  def ids: OpaqueIntSubset[ExprId]
}
