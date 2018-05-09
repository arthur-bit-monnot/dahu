package dahu.z3

import com.microsoft.z3._
import dahu.model.ir._
import dahu.model.math._
import dahu.recursion.FAlgebra
import cats.implicits._
import dahu.utils._
import dahu.model.functions._
import dahu.model.types.{Tag, TagIsoInt}

import scala.util.{Success, Try}

object Compiler {

  type PartialAlgebra = FAlgebra[Total, Expr]

  type Algebra = FAlgebra[Total, Try[Expr]]

  private def makeSafe(alg: PartialAlgebra): Algebra = {
    case x: InputF[_] => Try(alg(x))
    case x: CstF[_]   => Try(alg(x))
    case ITEF(cond, onTrue, onFalse, tpe) =>
      for {
        c <- cond
        t <- onTrue
        f <- onFalse
        res <- Try(alg(ITEF(c, t, f, tpe)))
      } yield res
    case ComputationF(f, args, t) =>
      args.toList.sequence
        .flatMap(as => Try(alg(ComputationF(f, as, t))))
    case ProductF(members, t) =>
      members.toList.sequence
        .flatMap(ms => Try(alg(ProductF(ms, t))))

  }

  object Ints {
    def unapply(args: Vec[Expr]): Option[List[IntExpr]] =
      if(args.forall(_.isInstanceOf[IntExpr]))
        Some(args.toList.asInstanceOf[List[IntExpr]])
      else
        None
  }
  object Bools {
    def unapply(args: Vec[Expr]): Option[List[BoolExpr]] =
      if(args.forall(_.isInstanceOf[BoolExpr]))
        Some(args.toList.asInstanceOf[List[BoolExpr]])
      else
        None
  }

  def algebra(ctx: Context): Algebra = makeSafe(partialAlgebra(ctx))

  def partialAlgebra(ctx: Context): PartialAlgebra = {
    case InputF(name, t) =>
      t match {
        case Tag.ofInt     => ctx.mkIntConst(name.toString)
        case Tag.ofBoolean => ctx.mkBoolConst(name.toString)
      }
    case CstF(value, t) =>
      t match {
        case Tag.ofInt     => ctx.mkInt(value.asInstanceOf[Int])
        case Tag.ofBoolean => ctx.mkBool(value.asInstanceOf[Boolean])
      }
    case ITEF(cond, onTrue, onFalse, t) =>
      cond match {
        case b: BoolExpr => ctx.mkITE(b, onTrue, onFalse)
      }

    case ComputationF(f: Fun1[_, _], Ints(lhs :: Nil), _) =>
      f match {
        case int.Negate => ctx.mkUnaryMinus(lhs)
      }
    case ComputationF(f: Fun1[_, _], Bools(lhs :: Nil), _) =>
      f match {
        case bool.Not => ctx.mkNot(lhs)
      }

    case ComputationF(f: Fun2[_, _, _], Ints(lhs :: rhs :: Nil), _) =>
      f match {
        case int.LEQ => ctx.mkLe(lhs, rhs)
        case int.EQ  => ctx.mkEq(lhs, rhs)
      }
    case ComputationF(f: Fun2[_, _, _], Bools(lhs :: rhs :: Nil), _) =>
      f match {
        case int.EQ => ctx.mkEq(lhs, rhs)
      }

    case ComputationF(f: FunN[_, _], Bools(args), t) =>
      f match {
        case bool.And => ctx.mkAnd(args: _*)
        case bool.Or  => ctx.mkOr(args: _*)
        case bool.XOr =>
          def xor(l: List[BoolExpr]): BoolExpr = l match {
            case Nil    => ctx.mkBool(false)
            case h :: t => ctx.mkXor(h, xor(t))
          }
          xor(args)
      }
    case ComputationF(f: FunN[_, _], Ints(args), t) =>
      f match {
        case int.Add   => ctx.mkAdd(args: _*)
        case int.Times => ctx.mkMul(args: _*)
      }

  }
}
