package dahu.model.compiler

import dahu.model.functions.{Fun, FunN}
import dahu.model.ir.{ComputationF, CstF, ExprF}
import dahu.model.math._
import dahu.model.types._
import dahu.recursion._
import dahu.utils.Errors._

object Optimizations {

  private val collapsable: Set[FunN[_, _]] = Set(bool.And, bool.Or)
  private def isCollapsable(op: Fun[_]): Boolean = op match {
    case x: FunN[_, _] => collapsable.contains(x)
    case _             => false
  }
  val flatten: ExprF[Fix[ExprF]] => ExprF[Fix[ExprF]] = {
    case ComputationF(operator, args, typ) if isCollapsable(operator) =>
      val flattenedArgs: Seq[Fix[ExprF]]= args.map(Fix.unfix).flatMap {
        case ComputationF(operator2, subargs, _) if operator2 == operator =>
          subargs
        case x: ExprF[Fix[ExprF]] => List(Fix(x))
      }
      ComputationF(operator, flattenedArgs, typ)
    case x => x
  }
  val constantElimination: ExprF[Fix[ExprF]] => ExprF[Fix[ExprF]] = {
    case ComputationF(bool.Not, Seq(CstF(true, t)), typ) =>
       CstF(Value(false), t)
    case ComputationF(bool.Not, Seq(CstF(false, t)), typ) =>
      CstF(Value(true), t)

    // todo: replace all following with eager evaluation when all arguments are constants
    case ComputationF(bool.And, Seq(), typ) =>
      CstF(Value(true), typ)
    case ComputationF(bool.Or, Seq(), typ) =>
      CstF(Value(false), typ)

    case ComputationF(int.EQ, Seq(CstF(v1, _), CstF(v2, _)), t) =>
      CstF(Value(v1 == v2), t)

    case ComputationF(int.LEQ, Seq(CstF(v1: Int, _), CstF(v2: Int, _)), t) =>
      CstF(Value(v1 <= v2), t)
    case x => x
  }
  val simplifyOr: ExprF[Fix[ExprF]] => ExprF[Fix[ExprF]] = {
    case ComputationF(bool.And, args, typ) if args.exists(isFalse) =>
      CstF(Value(false), typ)
    case ComputationF(bool.And, args, typ) =>
      ComputationF(bool.And, args.filterNot(isTrue), typ)

    case ComputationF(bool.Or, args, typ) if args.exists(isTrue) =>
      CstF(Value(true), typ)
    case ComputationF(bool.Or, args, typ) =>
      ComputationF(bool.Or, args.filterNot(isFalse), typ)
    case x => x
  }

  val passes = simplifyOr.andThen(constantElimination).andThen(flatten)

  def isTrue(f: Fix[ExprF]): Boolean = f match {
    case CstF(true, _) => true
    case _                  => false
  }
  def isFalse(f: Fix[ExprF]): Boolean = f match {
    case CstF(false, _) => true
    case _                   => false
  }

}
