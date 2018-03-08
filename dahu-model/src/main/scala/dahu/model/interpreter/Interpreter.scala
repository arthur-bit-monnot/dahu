package dahu.model.interpreter

import cats.implicits._
import dahu.model.ir._
import dahu.model.types.Value
import dahu.utils.errors._
import dahu.recursion._
import dahu.recursion.Recursion._

object Interpreter {

  def eval(ast: TotalSubAST[_])(root: ast.ID, inputs: ast.VID => Value): Value = {
    val input: InputF[_] => Value = {
      val map: Map[InputF[_], Value] =
        ast.variables.domain.toIterable().map(i => (ast.variables(i), Value(inputs(i)))).toMap
      x =>
        map(x)
    }
    val alg: FAlgebra[Total, Value] = {
      case x: InputF[_]             => input(x)
      case CstF(v, _)               => v
      case ComputationF(f, args, _) => Value(f.compute(args))
      case ProductF(members, t)     => Value(t.idProd.buildFromValues(members))
    }
    hylo(ast.tree.asFunction, alg)(root)
  }

  /** Evaluates the given AST with the provided inputs.
    * Returns Some(v), v being the value of the root node if all encountered constraints were satisfied.
    * Returns None, if a constraint (condition of a `SubjectTo(value, condition)` node) was encountered.
    * To extract the cause of a failure, look at evalWithFailureCause.
    */
  @deprecated
  def eval(ast: AST[_])(inputs: ast.VID => Value): Option[Value] = {
    val input: InputF[_] => Value = {
      val map: Map[InputF[_], Value] =
        ast.variables.domain.toIterable().map(i => (ast.variables(i), Value(inputs(i)))).toMap
      x =>
        map(x)
    }
    val alg: FAlgebra[ExprF, Option[Value]] = {
      case x: InputF[_] => Some(input(x))
      case CstF(v, _)   => Some(v)
      case ComputationF(f, args, _) =>
        val x: Option[List[Value]] = args.toList.sequence
        x match {
          case Some(actualArgs) => Some(Value(f.compute(actualArgs)))
          case None             => None
        }
      case ProductF(members, typ) =>
        val optMembers: Option[List[Value]] = members.toList.sequence
        optMembers
          .map(xs => typ.idProd.buildFromValues(xs))
          .map(Value(_))
      case Partial(value, cond, _) =>
        cond match {
          case Some(true)  => value
          case Some(false) => None
          case Some(x)     => unexpected(s"Condition does not evaluates to a boolean but to: $x")
          case None        => None
        }
      case OptionalF(value, cond, _) =>
        cond match {
          case Some(true)  => value.map(x => Value(Some(x)))
          case Some(false) => Some(Value(None))
          case Some(x)     => unexpected(s"Condition does not evaluates to a boolean but to: $x")
          case None        => None
        }
    }
    hylo(ast.tree.asFunction, alg)(ast.root)
  }

  sealed trait Result[+A] {
    def map[B](f: A => B): Result[B] = this match {
      case Res(v)                => Res(f(v))
      case ConstraintViolated(x) => ConstraintViolated(x)
      case Empty                 => Empty
    }
    def flatMap[B](f: A => Result[B]): Result[B] = this match {
      case Res(v)                => f(v)
      case ConstraintViolated(x) => ConstraintViolated(x)
      case Empty                 => Empty
    }
  }
  object Result {
    def pure[A](a: A): Result[A] = Res(a)
    def sequence[T](rs: Seq[Result[T]]): Result[Seq[T]] = {
      val l = rs.toList
      def go(current: Result[List[T]], pending: List[Result[T]]): Result[List[T]] = {
        pending match {
          case head :: tail =>
            val newCur = for {
              h <- head
              c <- current
            } yield h :: c
            go(newCur, tail)
          case Nil =>
            current.map(_.reverse) // todo: we should build the list in the correct order directly
        }
      }
      val res = go(pure(Nil), l)
      res
    }
  }
  case class ConstraintViolated(nodes: Seq[Any]) extends Result[Nothing]
  case class Res[T](v: T) extends Result[T]
  case object Empty extends Result[Nothing]
//  type Evaluation[Node, T] = Either[ConstraintViolated[Node], T]

  def evalWithFailureCause[T](ast: AST[T])(inputs: ast.VID => Value): Result[Value] = {
    val input: InputF[_] => Value = {
      val map: Map[InputF[_], Value] =
        ast.variables.domain.toIterable().map(i => (ast.variables(i), Value(inputs(i)))).toMap
      x =>
        map(x)
    }
    val alg: AttributeAlgebra[ast.ID, ExprF, Result[Value]] = {
      case EnvT(_, x: InputF[_]) => Res(input(x))
      case EnvT(_, CstF(v, _))   => Res(v)
      case EnvT(id, ComputationF(f, args, _)) =>
        Result
          .sequence(args)
          .map(actualArgs => Value(f.compute(actualArgs)))
      case EnvT(_, ProductF(members, t)) =>
        Result.sequence(members).map(ms => Value(t.idProd.buildFromValues(ms)))
      case EnvT(id, Partial(value, cond, _)) =>
        cond match {
          case Res(true) =>
            value
          case Res(false) =>
            ConstraintViolated(ast.toInput(id))
          case Res(x) => unexpected(s"Condition does not evaluates to a boolean but to: $x")
          case x      => x
        }
      case EnvT(_, OptionalF(value, present, _)) =>
        present match {
          case Res(true)  => value.map(x => Value(x))
          case Res(false) => Empty
          case Res(x)     => unexpected(s"Present does not evaluates to a boolean but to: $x")
          case x          => x
        }
    }
    val coalg: AttributeCoalgebra[ExprF, ast.ID] = ast.tree.asFunction.toAttributeCoalgebra
    hylo(coalg, alg)(ast.root)

  }

}
