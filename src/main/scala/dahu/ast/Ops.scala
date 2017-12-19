package dahu.ast

import cats.Functor
import dahu.arrows.{==>, Arrow, TypeInstances}
import dahu.expr.labels.Labels.Value
import dahu.recursion._

object Ops {

  val toTable: dahu.expr.Expr[Any] ==> ASTable = Arrow.lift((x: dahu.expr.Expr[Any]) => {
    val (headInt, tableInt) = Algebras.encodeAsPair(x)
    new ASTable {
      private val vec: Array[ExprF[EId]] = tableInt.asInstanceOf[Vector[Expr]].toArray

      override val root: EId = headInt.asInstanceOf[EId]

      override val arrow: EId ==> Expr = Arrow.lift(x => vec(EId.unwrap(x)))

      override def ids: TypeInstances[EId] = new TypeInstances[EId] {
        override val enumerate: Array[EId] = vec.indices.toArray.asInstanceOf[Array[EId]]
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

  def asRecursiveFold[X](ast: ASTable)(alg: Algebra[ExprF, X])(
      implicit F: Functor[ExprF]): ast.EId ==> X = {
    def go(id: ast.EId): X =
      alg(F.map(ast.arrow(id))(go))
    Arrow.lift(go)
  }

}
