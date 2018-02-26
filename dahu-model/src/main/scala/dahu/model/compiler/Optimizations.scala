package dahu.model.compiler

import dahu.model.functions.{Fun, FunN}
import dahu.model.ir.{ComputationF, CstF, ExprF, Total}
import dahu.model.math._
import dahu.model.types._
import dahu.recursion._
import dahu.utils.errors._

object Optimizations {

  private val collapsable: Set[FunN[_, _]] = Set(bool.And, bool.Or)
  private def isCollapsable(op: Fun[_]): Boolean = op match {
    case x: FunN[_, _] => collapsable.contains(x)
    case _             => false
  }
  val flatten: ExprF[Fix[ExprF]] => ExprF[Fix[ExprF]] = {
    case ComputationF(operator, args, typ) if isCollapsable(operator) =>
      val flattenedArgs: Seq[Fix[ExprF]] = args.map(Fix.unfix).flatMap {
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
    case _             => false
  }
  def isFalse(f: Fix[ExprF]): Boolean = f match {
    case CstF(false, _) => true
    case _              => false
  }

  type Tree = Total[Fix[Total]]
  type PASS = Tree => Tree

  val elimIdentity: PASS = {
    case ComputationF(f: Monoid[_], args, t) =>
      ComputationF(f, args.filterNot(_.unfix == f.liftedIdentity), t)
    case x => x
  }

  val elimEmptyMonoids: PASS = {
    case ComputationF(f: Monoid[_], Seq(), _) => f.liftedIdentity
    case x                                    => x
  }
  private val FALSE: Fix[Total] = Fix(CstF(Value(false), Tag.ofBoolean))
  private val TRUE: Fix[Total] = Fix(CstF(Value(true), Tag.ofBoolean))
  val constantFolding: PASS = {
    case ComputationF(bool.And, args, _) if args.contains(FALSE) => FALSE.unfix
    case ComputationF(bool.Or, args, _) if args.contains(TRUE)   => TRUE.unfix

    // commutative monoid, evaluate the combination of all constants args
    case ComputationF(f: CommutativeMonoid[_], args, t) =>
      // partition between unevaluated and constants
      val (vars, csts) = args.foldLeft((Seq[Fix[Total]](), Seq[Value]())) {
        case ((vs, cs), CstF(v, _)) => (vs, cs :+ v)
        case ((vs, cs), x)          => (vs :+ x, cs)
      }
      val evalOfConstants = CstF[Fix[Total]](Value(f.compute(csts)), f.tpe)
      if(vars.isEmpty) {
        // no unevaluated terms, return results
        evalOfConstants
      } else {
        // some unevaluated terms, return partially evaluated computation
        ComputationF(f, vars :+ Fix(evalOfConstants), t)
      }

    // any function, evaluate if all args are constant
    case ComputationF(f, args, t) if args.forall(_.isInstanceOf[CstF[_]]) =>
      val params = args.map {
        case CstF(value, _) => value
        case _              => unexpected
      }
      CstF(Value(f.compute(params)), t)
    case x => x
  }

  val flattenMonoid: PASS = {
    case ComputationF(f: Monoid[_], args, t) =>
      val flatArgs: Seq[Fix[Total]] = args.map(_.unfix).flatMap {
        case ComputationF(g, subargs, t2) if f == g && t == t2 =>
          subargs
        case x => Seq(Fix(x))
      }
      ComputationF(f, flatArgs, t)
    case x => x
  }

  val simplifications: Seq[PASS] = Seq(
    elimIdentity,
    elimEmptyMonoids,
    flattenMonoid,
    constantFolding
  )
  val simplificationAlgebra: Total[Fix[Total]] => Fix[Total] =
    simplifications
      .fold((x: Tree) => x)(_ andThen _)
      .andThen(x => Fix(x))
}
