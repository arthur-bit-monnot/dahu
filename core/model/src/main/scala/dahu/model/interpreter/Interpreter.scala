package dahu.model.interpreter

import cats.implicits._
import cats.kernel.Monoid
import dahu.utils._
import dahu.model.input.{Lambda, TypedIdent}
import dahu.model.ir._
import dahu.model.types._
import dahu.utils.errors._
import dahu.recursion._
import dahu.recursion.Recursion._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Interpreter {

  def evalAlgebra: FAlgebra[Total, PEval[Any]] = {
    case InputF(id, _) =>
      FEval("aaaaa") //TODO
//      Unknown(Set(id))
    case CstF(v, _) => FEval(v)
    case ComputationF(f, args, _) =>
      args.ssequence
        .smap(as => Value(f.computeFromAny(as)))
    case SequenceF(members, _) => members.ssequence
    case ProductF(members, t) =>
      members.ssequence
        .smap(as => t.fromValues(as))
    case NoopF(x, _) => x
    case ITEF(cond, onTrue, onFalse, _) =>
      Vec(cond, onTrue, onFalse).ssequence
        .smap {
          case Vec(c, t, f) =>
            c match {
              case true  => t
              case false => f
              case _     => unexpected
            }
          case _ => unexpected
        }
    case LambdaParamF(id, tpe) => LambdaParamPlaceHolder(id)
    case LambdaF(in, tree, id, tpe) =>
      FEval(null) //TODO
//      PEFunc(id, tree)
  }

  def evalAlgebra(valueOf: TypedIdent => Option[Value]): FAlgebra[Total, PEval[Any]] = {
    case InputF(id, _) =>
      valueOf(id) match {
        case Some(v) => FEval(v)
        case None    => Unknown(Set(id))
      }
    case CstF(v, _) => FEval(v)
    case ComputationF(f, args, _) =>
      args.ssequence
        .smap(as => Value(f.computeFromAny(as)))
    case SequenceF(members, _) => members.ssequence
    case ProductF(members, t) =>
      members.ssequence
        .smap(as => t.fromValues(as))
    case NoopF(x, _) => x
    case ITEF(cond, onTrue, onFalse, _) =>
      Vec(cond, onTrue, onFalse).ssequence
        .smap {
          case Vec(c, t, f) =>
            c match {
              case true  => t
              case false => f
              case _     => unexpected
            }
          case _ => unexpected
        }
    case LambdaParamF(id, tpe)      => LambdaParamPlaceHolder(id)
    case LambdaF(in, tree, id, tpe) => PEFunc(id, tree)
  }

  @deprecated("use the one in LambdaInterpreter instead", since = "now")
  def partialEvalAlgebra(valueOf: TypedIdent => Value): FAlgebra[Total, Result[Value]] = {
    case InputF(id, _) => Res(valueOf(id))
    case CstF(v, _)    => Res(v)
    case NoopF(v, _)   => v
    case ComputationF(f, args, _) =>
      Result
        .sequence(args)
        .map(as => Value(f.compute(as)))
    case ProductF(members, t) =>
      Result
        .sequence(members)
        .map(as => Value(t.buildFromValues(as)))
    case SequenceF(members, t) =>
      Result.sequence(members).map(Value(_))
    case ITEF(cond, onTrue, onFalse, _) =>
      cond.flatMap {
        case true  => onTrue
        case false => onFalse
        case _     => dahu.utils.errors.unexpected
      }
    case _: LambdaF[_]      => ??? // TODO: generation of functions from lambda not implemented yet
    case _: LambdaParamF[_] => ???
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
      case EnvT(_, ComputationF(f, args, _)) =>
        Result
          .sequence(args)
          .map(actualArgs => Value(f.compute(actualArgs)))
      case EnvT(_, ProductF(members, t)) =>
        Result.sequence(members).map(ms => Value(t.buildFromValues(ms)))
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
