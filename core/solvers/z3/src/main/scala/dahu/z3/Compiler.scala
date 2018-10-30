package dahu.z3

import com.microsoft.z3._
import dahu.model.ir._
import dahu.model.math._
import dahu.recursion.FAlgebra
import cats.implicits._
import dahu.utils._
import dahu.model.functions._
import dahu.model.types.{Bool, Tag, TagIsoInt}

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
      args.ssequence
        .flatMap(as => Try(alg(ComputationF(f, as, t))))
    case ProductF(members, t) =>
      members.ssequence
        .flatMap(ms => Try(alg(ProductF(ms, t))))
    case SequenceF(members, t) =>
      members.ssequence
        .flatMap(ms => Try(alg(SequenceF(ms, t))))

  }

  object Ints {
    def unapplySeq(args: Vec[Expr]): Option[IndexedSeq[IntExpr]] =
      if(args.forall(_.isInstanceOf[IntExpr]))
        Some(args.asInstanceOf[Vec[IntExpr]].toSeq)
      else
        None
  }
  object Bools {
    def unapplySeq(args: Vec[Expr]): Option[IndexedSeq[BoolExpr]] =
      if(args.forall(_.isInstanceOf[BoolExpr]))
        Some(args.asInstanceOf[Vec[BoolExpr]].toSeq)
      else
        None
  }

  def algebra(ctx: Context): Algebra = makeSafe(partialAlgebra(ctx))

  def partialAlgebra(ctx: Context): PartialAlgebra = {
    case InputF(name, t) =>
      t match {
        case _ if t.isInt     => ctx.mkIntConst(name.toString)
        case _ if t.isBoolean => ctx.mkBoolConst(name.toString + ";")
      }
    case CstF(value: Int, t) =>
      t match {
        case _ if t.isInt => ctx.mkInt(value.asInstanceOf[Int])
        case _ if t.isBoolean =>
          assert(t eq Tag.ofBoolean)
          ctx.mkBool(if(value == Bool.True) true else false)
      }
    case ITEF(cond, onTrue, onFalse, t) =>
      cond match {
        case b: BoolExpr => ctx.mkITE(b, onTrue, onFalse)
      }

    case ComputationF(f: Fun1[_, _], Ints(lhs), _) =>
      f match {
        case int.Negate => ctx.mkUnaryMinus(lhs)
      }
    case ComputationF(f: Fun1[_, _], Bools(lhs), _) =>
      f match {
        case bool.Not => ctx.mkNot(lhs)
      }

    case ComputationF(f: Fun2[_, _, _], Ints(lhs, rhs), _) =>
      f match {
        case int.LEQ => ctx.mkLe(lhs, rhs)
        case int.EQ  => ctx.mkEq(lhs, rhs)
      }
    case ComputationF(f: Fun2[_, _, _], Bools(lhs, rhs), _) =>
      f match {
        case int.EQ => ctx.mkEq(lhs, rhs)
      }

    case ComputationF(f: FunN[_, _], Bools(args @ _*), _) =>
      f match {
        case bool.And => ctx.mkAnd(args: _*)
        case bool.Or  => ctx.mkOr(args: _*)
        case bool.XOr => args.fold(ctx.mkBool(false))(ctx.mkXor)
      }
    case ComputationF(f: FunN[_, _], Ints(args @ _*), _) =>
      f match {
        case int.Add   => ctx.mkAdd(args: _*)
        case int.Times => ctx.mkMul(args: _*)
      }

  }
}
