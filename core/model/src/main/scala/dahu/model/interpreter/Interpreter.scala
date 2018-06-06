package dahu.model.interpreter

import cats.Foldable
import cats.implicits._
import cats.kernel.Monoid
import dahu.utils._
import dahu.model.input.{Ident, Present, TypedIdent}
import dahu.model.ir._
import dahu.model.types._
import dahu.utils.errors._
import dahu.recursion._
import dahu.recursion.Recursion._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

object Interpreter {

  def evalAlgebra(valueOf: TypedIdent[Any] => Value): FAlgebra[Total, Value] = {
    case InputF(id, _)            => valueOf(id)
    case CstF(v, _)               => v
    case ComputationF(f, args, _) => Value(f.compute(args))
    case ProductF(members, t)     => Value(t.idProd.buildFromValues(members))
    case ITEF(cond, onTrue, onFalse, _) =>
      cond match {
        case true  => onTrue
        case false => onFalse
        case _     => dahu.utils.errors.unexpected
      }
    case SequenceF(values, _) => Value(values)
  }

  def partialEvalAlgebra(valueOf: TypedIdent[Any] => Value): FAlgebra[NoApplyF, Result[Value]] = {
    case InputF(id, _) => Res(valueOf(id))
    case CstF(v, _)    => Res(v)
    case ComputationF(f, args, _) =>
      Result
        .sequence(args)
        .map(as => Value(f.compute(as)))
    case ProductF(members, t) =>
      Result
        .sequence(members)
        .map(as => Value(t.idProd.buildFromValues(as)))
    case SequenceF(members, t) =>
      Result.sequence(members).map(Value(_))
    case ITEF(cond, onTrue, onFalse, _) =>
      cond.flatMap {
        case true  => onTrue
        case false => onFalse
        case _     => dahu.utils.errors.unexpected
      }
    case Partial(value, cond, _) =>
      cond match {
        case Empty              => value
        case Res(true)          => value
        case Res(false)         => ConstraintViolated
        case Res(x)             => unexpected(s"Condition does not evaluates to a boolean but to: $x")
        case ConstraintViolated => ConstraintViolated
      }
    case OptionalF(value, present, _) =>
      present match {
        case Res(true)          => value
        case Res(false)         => Empty
        case Res(x)             => unexpected(s"Present does not evaluates to a boolean but to: $x")
        case Empty              => ???
        case ConstraintViolated => ConstraintViolated
      }
    case PresentF(v) =>
      v match {
        case Empty => Res(Value(false)) //TODO, semantics for ConstraintViolated
        case _     => Res(Value(true))
      }
    case ValidF(v) =>
      v match {
        case ConstraintViolated => Res(Value(false)) //TODO: semantics for Empty
        case _                  => Res(Value(true))
      }
    case _: LambdaF[_]      => ??? // TODO: generation of functions from lambda not implemented yet
    case _: LambdaParamF[_] => ???
  }

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
      case SequenceF(members, _)    => Value(members)
      case ProductF(members, t)     => Value(t.idProd.buildFromValues(members))
      case ITEF(cond, onTrue, onFalse, _) =>
        cond match {
          case true  => onTrue
          case false => onFalse
          case _     => unexpected
        }
    }
    hylo(ast.tree.asFunction, alg)(root)
  }

  /** Evaluates the given AST with the provided inputs.
    * Returns Some(v), v being the value of the root node if all encountered constraints were satisfied.
    * Returns None, if a constraint (condition of a `SubjectTo(value, condition)` node) was encountered.
    * To extract the cause of a failure, look at evalWithFailureCause.
    */
  def eval(ast: AST[_])(inputs: ast.VID => Value): Option[Value] = {
    evalWithFailureCause(ast)(inputs) match {
      case Res(x) => Some(x)
      case _      => None
    }
  }

  sealed trait Result[+A] {
    def map[B](f: A => B): Result[B] = this match {
      case Res(v)             => Res(f(v))
      case ConstraintViolated => ConstraintViolated
      case Empty              => Empty
    }
    def flatMap[B](f: A => Result[B]): Result[B] = this match {
      case Res(v)             => f(v)
      case ConstraintViolated => ConstraintViolated
      case Empty              => Empty
    }
  }
  object Result {
    def pure[A](a: A): Result[A] = Res(a)
    def sequence[T: ClassTag](rs: Vec[Result[T]]): Result[Vec[T]] = {
      val l = rs.toList
      @tailrec def go(current: Result[List[T]], pending: List[Result[T]]): Result[List[T]] = {
        pending match {
          case head :: tail =>
            val next = (current, head) match {
              case (Empty, _)              => Empty
              case (_, Empty)              => Empty
              case (ConstraintViolated, _) => ConstraintViolated
              case (_, ConstraintViolated) => ConstraintViolated
              case (Res(list), Res(h))     => Res(h :: list)
            }
            go(next, tail)
          case Nil =>
            current.map(_.reverse) // todo: we should build the list in the correct order directly
        }
      }
      val res = go(pure(Nil), l)
      res.map(Vec.fromSeq(_))
    }
    implicit def monoidInstance[T: Monoid](): Monoid[Result[T]] = new Monoid[Result[T]] {
      override def empty: Result[T] = Res(Monoid[T].empty)

      override def combine(x: Result[T], y: Result[T]): Result[T] = (x, y) match {
        case (Empty, _)              => Empty
        case (_, Empty)              => Empty
        case (ConstraintViolated, _) => ConstraintViolated
        case (_, ConstraintViolated) => ConstraintViolated
        case (Res(xr), Res(yr))      => Res(xr |+| yr)
      }
    }
  }
  case object ConstraintViolated extends Result[Nothing]

  case class Res[T](v: T) extends Result[T]
  case object Empty extends Result[Nothing]

  def evalWithFailureCause[T](ast: AST[T])(inputs: ast.VID => Value): Result[Value] = {
    val input: InputF[_] => Value = {
      val map: Map[InputF[_], Value] =
        ast.variables.domain.toIterable().map(i => (ast.variables(i), Value(inputs(i)))).toMap
      x =>
        map(x)
    }
    // TODO: the attribute algebra is not needed anymore (EnvT wrapper is useless)
    // should use the partialEvalAlgebra instead
    val alg: AttributeAlgebra[ast.ID, ExprF, Result[Value]] = {
      case EnvT(_, x: InputF[_]) => Res(input(x))
      case EnvT(_, CstF(v, _))   => Res(v)
      case EnvT(_, PresentF(v)) =>
        v match {
          case Empty => Res(Value(false))
          case _     => Res(Value(true))
        }
      case EnvT(_, ValidF(v)) =>
        v match {
          case ConstraintViolated => Res(Value(false))
          case _                  => Res(Value(true))
        }
      case EnvT(_, ComputationF(f, args, _)) =>
        Result
          .sequence(args)
          .map(actualArgs => Value(f.compute(actualArgs)))
      case EnvT(_, ProductF(members, t)) =>
        Result.sequence(members).map(ms => Value(t.idProd.buildFromValues(ms)))
      case EnvT(id, Partial(value, cond, _)) =>
        cond match {
          case Empty =>
            value
          case Res(true) =>
            value
          case Res(false) =>
            ConstraintViolated

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
      case EnvT(_, ITEF(cond, onTrue, onFalse, _)) =>
        cond.flatMap {
          case true  => onTrue
          case false => onFalse
        }
      case _ => ??? // this evaluation method is outdated and deprecated
    }

    val coalg: AttributeCoalgebra[ExprF, ast.ID] = ast.tree.asFunction.toAttributeCoalgebra
    hylo(coalg, alg)(ast.root)

  }

}
