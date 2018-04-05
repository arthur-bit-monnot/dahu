package dahu.model.compiler

import dahu.model.functions.{Fun, FunN}
import dahu.model.ir.{ComputationF, CstF, ExprF, Total}
import dahu.model.math._
import dahu.model.types._
import dahu.recursion._
import dahu.utils.errors._

import scala.collection.mutable.ArrayBuffer

object Optimizations {

  type Tree = Total[Fix[Total]]
  type PASS = Tree => Tree

  object Passes {

    /** Assigns a name to a pass by wrapping it into new Object with overloaded toString. */
    private def namedPass(name: String)(pass: PASS): PASS = new Function[Tree, Tree] {
      override def apply(v1: Tree): Tree = pass(v1)
      override def toString(): String = name
    }

    val elimIdentity: PASS = namedPass("elim-identity") {
      case ComputationF(f: Monoid[_], args, t) =>
        ComputationF(f, args.filterNot(_.unfix == f.liftedIdentity), t)
      case x => x
    }

    val elimEmptyMonoids: PASS = namedPass("elim-empty-monoid") {
      case x @ ComputationF(f: Monoid[_], Seq(), _) => f.liftedIdentity
      case x                                        => x
    }
    private val FALSE: Fix[Total] = Fix(CstF(Value(false), Tag.ofBoolean))
    private val TRUE: Fix[Total] = Fix(CstF(Value(true), Tag.ofBoolean))
    val constantFolding: PASS = namedPass("constant-folding") {
      case ComputationF(bool.And, args, _) if args.contains(FALSE) => FALSE.unfix
      case ComputationF(bool.Or, args, _) if args.contains(TRUE)   => TRUE.unfix

      // commutative monoid, evaluate the combination of all constants args
      case ComputationF(f: CommutativeMonoid[_], args, t) =>
        // partition between unevaluated and constants
        val (vars, csts) = args.foldLeft((List[Fix[Total]](), List[Value]())) {
          case ((vs, cs), CstF(v, _)) => (vs, v :: cs)
          case ((vs, cs), x)          => (x :: vs, cs)
        }
        val evalOfConstants = CstF[Fix[Total]](Value(f.compute(csts)), f.tpe)
        if(vars.isEmpty) {
          // no unevaluated terms, return results
          evalOfConstants
        } else {
          // some unevaluated terms, return partially evaluated computation
          ComputationF(f, Fix(evalOfConstants) :: vars, t)
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

    val flattenMonoid: PASS = namedPass("flatten-monoid") {
      case ComputationF(f: Monoid[_], args, t) =>
        val flatArgs = ArrayBuffer[Fix[Total]]()
        for(a <- args) {
          a.unfix match {
            case ComputationF(g, subargs, t2) if f == g && t == t2 =>
              flatArgs ++= subargs
            case x =>
              flatArgs += Fix(x)
          }
        }
        ComputationF(f, flatArgs, t)
      case x => x
    }

    val elimSingletonAndOr: PASS = namedPass("elim-singleton-and-or") {
      case ComputationF(bool.And, Seq(arg), _) => arg.unfix
      case ComputationF(bool.Or, Seq(arg), _)  => arg.unfix
      case x                                   => x
    }

    val elimDuplicatesAndOr: PASS = namedPass("elim-duplicates-and-or") {
      case ComputationF(bool.And, args, tpe) => ComputationF(bool.And, args.distinct, tpe)
      case ComputationF(bool.Or, args, tpe)  => ComputationF(bool.Or, args.distinct, tpe)
      case x                                 => x
    }

    val elimTautologies: PASS = namedPass("elim-tautologies") {
      case ComputationF(int.LEQ, Seq(a1, a2), _) if a1 == a2 => TRUE.unfix
      case ComputationF(int.EQ, Seq(a1, a2), _) if a1 == a2  => TRUE.unfix
      case x                                                 => x
    }
  }

  import Passes._
  val simplifications: Seq[PASS] = Seq(
    elimIdentity,
    elimEmptyMonoids,
    flattenMonoid,
    constantFolding,
    elimTautologies,
    elimSingletonAndOr,
    elimDuplicatesAndOr,
    // repeated
    elimIdentity,
    elimSingletonAndOr,
  )
  val simplificationAlgebra: Total[Fix[Total]] => Fix[Total] =
    simplifications
      .fold((x: Tree) => x)(_ andThen _)
      .andThen(x => Fix(x))
}
