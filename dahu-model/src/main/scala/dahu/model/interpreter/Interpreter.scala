package dahu.model.interpreter

import cats._
import cats.implicits._
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
  case class ConstraintViolated(x: String)
  type Evaluation[T] = Either[ConstraintViolated, T]
  def eval(ast: AST[_])(inputs: ast.VID => Value): Evaluation[Value] = {
    val input: InputF[_] => Value = {
      val map: Map[InputF[_], Value] =
        ast.variables.domain.toIterable().map(i => (ast.variables(i), Value(inputs(i)))).toMap
      x =>
        map(x)
    }
    val alg: Algebra[ExprF, Evaluation[Value]] = {
      case x: InputF[_] => Right(input(x))
      case CstF(v, _)   => Right(v)
      case ComputationF(f, args, _) =>
        val x: Evaluation[List[Value]] = args.toList.sequence
        x match {
          case Right(actualArgs) => Right(Value(f.compute(actualArgs)))
          case Left(x)           => Left(x)
        }
      case ProductF(members, _)       => members.toList.sequence.map(Value(_))
      case SubjectToF(value, cond, _) => cond.flatMap(_ => value)
    }
    val hyl = hylo(ast.tree.asFunction, alg)
    hyl(ast.root)
  }

}
