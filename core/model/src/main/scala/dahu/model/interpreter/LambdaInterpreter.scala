package dahu.model.interpreter

import cats._
import cats.implicits._
import dahu.graphs.TreeNode
import dahu.model.input.{Ident, Lambda}
import dahu.model.ir._
import dahu.model.types._
import dahu.recursion.FAlgebra
import dahu.utils._
import dahu.utils.SFunctor._
import dahu.utils.errors._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object LambdaInterpreter {

  sealed trait Result[+A] {
    def map[B](f: A => B): Result[B] = this match {
      case Res(v)             => Res(f(v))
      case ConstraintViolated => ConstraintViolated
      case Empty              => Empty
      case Pending(_, _)      => unexpected
    }
    def flatMap[B](f: A => Result[B]): Result[B] = this match {
      case Res(v)             => f(v)
      case ConstraintViolated => ConstraintViolated
      case Empty              => Empty
      case Pending(_, _)      => unexpected
    }

  }
  object Result {
    def pure[A](a: A): Result[A] = Res(a)

    def eval[A](fa: Result[A])(f: StaticF[Result[A]] => Result[A]): Result[A] = fa match {
      case Res(v)                 => Res(v)
      case ConstraintViolated     => ConstraintViolated
      case Empty                  => Empty
      case Pending(static, stack) => f(static.smap(eval[A](_)(f)))
    }

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
              case (Pending(_, _), _)      => unexpected
              case (_, Pending(_, _))      => unexpected
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
        case (Pending(_, _), _)      => unexpected
        case (_, Pending(_, _))      => unexpected
      }
    }
  }

  case object ConstraintViolated extends Result[Nothing]
  case class Res[T](v: T) extends Result[T]
  case class Pending[T](e: StaticF[Result[T]], stack: List[Lambda.LambdaIdent]) extends Result[T]
  case object Empty extends Result[Nothing]

  def partialEvalAlgebra2(valueOf: Ident => Option[Value]): FAlgebra[StaticF, Result[Value]] =
    partialEvalAlgebra(valueOf, _ => None)

  private def partialEvalAlgebra(
      valueOf: Ident => Option[Value],
      lambdaParamsBind: Lambda.LambdaIdent => Option[Value]): FAlgebra[StaticF, Result[Value]] =
    e => {
      val res = e match {

        case ApplyF(lbd @ Pending(_, in :: tail), Res(v), _) =>
          val f: Lambda.LambdaIdent => Option[Value] = x => {
            if(x == in) Some(v)
            else lambdaParamsBind(x)
          }
          val tmp = Result.eval[Value](lbd)(partialEvalAlgebra(valueOf, f))
          tmp match {
            case Pending(x, y) => Pending(x, tail)
            case x             => x
          }

        case ApplyF(Res(value), Res(_), _) => Res(value)
        case ApplyF(_, _, _)               => ???

        case LambdaF(Pending(LambdaParamF(id, _), Nil), Pending(tree, callStack), id2, _) =>
          assert(id == id2)
          //      assert(callStack.isEmpty)
          Pending(tree, id :: callStack)
        case LambdaF(Res(_), Res(v), _, _) => Res(v)
        case LambdaF(_, _, _, _)           => ???

        case x: LambdaParamF[_] =>
          lambdaParamsBind(x.id) match {
            case Some(v) => Res(v)
            case None    => Pending(x, Nil)
          }

        case x if TreeNode[StaticF].children(x).exists(_.isInstanceOf[Pending[Value]]) =>
          val tmp = TreeNode[StaticF].children(x).collect { case x @ Pending(_, _) => x }
          assert(tmp.forall(_.stack.isEmpty))
          Pending(x, Nil)
        case x @ InputF(id, _) =>
          valueOf(id) match {
            case Some(v) => Res(v)
            case None    => Pending(x, Nil)
          }
        case CstF(v, _) => Res(v)
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
            case ConstraintViolated => ConstraintViolated
            case Res(true)          => value
            case Res(false)         => ConstraintViolated
            case Res(x)             => unexpected(s"Condition does not evaluates to a boolean but to: $x")
            case Pending(_, _)      => unexpected
          }
        case OptionalF(value, present, _) =>
          present match {
            case Empty              => value
            case ConstraintViolated => ConstraintViolated
            case Res(true)          => value
            case Res(false)         => Empty
            case Res(x)             => unexpected(s"Present does not evaluates to a boolean but to: $x")
            case Pending(_, _)      => unexpected
          }
        case PresentF(v) =>
          v match {
            case Empty              => Res(Value(false))
            case ConstraintViolated => ConstraintViolated
            case Res(_)             => Res(Value(true))
            case Pending(_, _)      => unexpected
          }
        case ValidF(v) =>
          v match {
            case Empty              => Res(Value(true))
            case ConstraintViolated => Res(Value(false))
            case Res(_)             => Res(Value(true))
            case Pending(_, _)      => unexpected
          }
      }
      res
    }

}
