package dahu.ast

import cats.Functor
import dahu.arrows.{==>, Arrow, OpaqueIntSubset, TypeInstances}
import dahu.expr.labels.Labels.Value
import dahu.recursion._
import spire.ClassTag

import scala.collection.mutable

object Ops {

  val toTable: dahu.expr.Expr[Any] ==> ASTable = Arrow.lift((x: dahu.expr.Expr[Any]) => {
    val (headInt, tableInt) = Algebras.encodeAsPair(x)
    new ASTable {
      private val vec: Array[ExprF[EId]] = tableInt.asInstanceOf[Vector[Expr]].toArray

      override val root: EId = headInt.asInstanceOf[EId]

      override val arrow: EId ==> Expr = Arrow.lift(x => vec(EId.unwrap(x)))

      override val ids: OpaqueIntSubset[EId] = new OpaqueIntSubset[EId] {
        override def first: EId = EId(0)
        override def last       = EId(tableInt.size - 1)

        override def wrap(i: Int): EId                 = EId(i)
        override def unwrap(a: EId): Int               = EId.unwrap(a)
        override def subst[F[_]](fi: F[Int]): F[EId]   = EId.subst(fi)
        override def unsubst[F[_]](fa: F[EId]): F[Int] = EId.unsubst(fa)

        override val enumerate: Array[EId] = EId.subst(tableInt.indices.toArray)
      }
    }
  })

  def extractType[X]: ExprF[X] ==> String = Arrow.lift(_.typ.toString)

  def types(ast: ASTable): ast.EId ==> String = ast.arrow.andThen(extractType)

  def evaluator(ast: ASTable)(inputs: ast.Variable ==> Value): ast.EId ==> Value = {
    def go(i: ast.EId): Value = {
      ast.arrow(i) match {
        case CstF(v, _)               => v
        case x @ InputF(_, _)         => inputs(x)
        case ComputationF(f, args, _) => Value(f.compute(args.map(go)))
      }
    }
    Arrow.lift(go)
  }

  type Algebra[F[_], X] = F[X] ==> X
  def evalAlgebra(ast: ASTable)(inputs: ast.Variable ==> Value): Algebra[ExprF, Value] =
    Arrow.lift {
      case CstF(v, _)               => v
      case x @ InputF(_, _)         => inputs(x)
      case ComputationF(f, args, _) => Value(f.compute(args))
    }

  def cata[X](ast: ASTable)(alg: Algebra[ExprF, X])(implicit F: Functor[ExprF]): ast.EId ==> X = {
    def go(id: ast.EId): X =
      alg(F.map(ast.arrow(id))(go))
    Arrow.lift(go)
  }

  def memoizedCata[X](ast: ASTable)(
      alg: Algebra[ExprF, X])(implicit F: Functor[ExprF], classTag: ClassTag[X]): ast.EId ==> X = {
    val cache = ast.ids.newCache[X]
    def go(id: ast.EId): X =
      cache.getOrElseUpdate(id, alg(F.map(ast.arrow(id))(go)))

    Arrow.lift(go)
  }

}
