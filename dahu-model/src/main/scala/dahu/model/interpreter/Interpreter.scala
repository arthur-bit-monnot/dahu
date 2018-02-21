package dahu.model.interpreter

import cats.Functor
import dahu.model.ir._
import dahu.model.types.Value

object Interpreter {

  type Algebra[F[_], X] = F[X] => X
  type Coalgebra[F[_], X] = X => F[X]

  def hylo[A, B, F[_]](coalgebra: Coalgebra[F, A], algebra: Algebra[F, B])(
      implicit F: Functor[F]): A => B = {
    def go(id: A): B =
      algebra(F.map(coalgebra(id))(go))
    go
  }

  def eval(ast: AST[_])(inputs: ast.VID => Value): Value = {
    val input: InputF[_] => Value = {
      val map: Map[InputF[_], Value] =
        ast.variables.domain.toIterable().map(i => (ast.variables(i), Value(inputs(i)))).toMap
      x =>
        map(x)
    }
    val alg: Algebra[ExprF, Value] = {
      case x: InputF[_]             => input(x)
      case CstF(v, _)               => v
      case ComputationF(f, args, _) => Value(f.compute(args))
      case ProductF(members, _)     => Value(members.toList)
    }
    val hyl = hylo(ast.tree.asFunction, alg)
    hyl(ast.root)
  }

}
