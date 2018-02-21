package dahu.ast

import cats.Functor
import dahu.maps._
import dahu.maps.memoization.Cache
import dahu.maps.recursion.Algebra
import dahu.recursion._
import spire.ClassTag

import scala.collection.mutable

object Ops {

  val toTable: dahu.expr.Expr[Any] ==> ASTable = Arrow.lift((x: dahu.expr.Expr[Any]) => {
    val (headInt, tableInt) = Algebras.encodeAsPair(x)
    new ASTable {
      private val vec: Array[ExprF[EId]] = tableInt.asInstanceOf[Vector[Expr]].toArray

      override val root: EId = EId(headInt)

      override val coalgebra: EId ==> Expr = Arrow.lift(x => vec(EId.toInt(x)))

      override val ids: OpaqueIntSubset[EId] = new OpaqueIntSubset[EId] {
        override def wrap(i: Int): EId = EId.fromInt(i)
        override def unwrap(a: EId): Int = EId.toInt(a)
        override def subst[F[_]](fi: F[Int]): F[EId] = EId.fromIntF(fi)
        override def unsubst[F[_]](fa: F[EId]): F[Int] = EId.toIntF(fa)

        override val enumerate: Array[EId] = subst(tableInt.indices.toArray)
      }

      override val variableIds: OpaqueIntSubset[VarId] = new OpaqueIntSubset[VarId] {
        override def wrap(i: Int): VarId = VarId.fromInt(i)
        override def unwrap(a: VarId): Int = VarId.toInt(a)
        override def subst[F[_]](fi: F[Int]): F[VarId] = VarId.fromIntF(fi)
        override def unsubst[F[_]](fa: F[VarId]): F[Int] = VarId.toIntF(fa)

        override val enumerate: Array[VarId] =
          subst(tableInt.indices.filter(vec(_).isInstanceOf[Variable]).toArray)
      }
    }
  })

  def extractType[X]: ExprF[X] ==> Type = Arrow.lift(_.typ)

  def types(ast: ASTable): ast.EId ==> Type = ast.coalgebra.andThen(extractType)

  def evaluator(ast: ASTable)(inputs: ast.Variable ==> Value): ast.EId ==> Value = {
    def go(i: ast.EId): Value = {
      ast.coalgebra(i) match {
        case CstF(v, _)               => v
        case x @ InputF(_, _)         => inputs(x)
        case ComputationF(f, args, _) => Value(f.compute(args.map(go)))
      }
    }
    Arrow.lift(go)
  }

  def evalAlgebra(ast: ASTable)(inputs: ast.Variable ==> Value): Algebra[ExprF, Value] =
    Arrow.lift {
      case CstF(v, _)               => v
      case x @ InputF(_, _)         => inputs(x)
      case ComputationF(f, args, _) => Value(f.compute(args))
    }

}
