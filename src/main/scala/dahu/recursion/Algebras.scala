package dahu.recursion

import dahu.expr._
import dahu.recursion.TypeAlias._
import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

import scala.reflect.runtime.universe.{Type, TypeTag, typeOf}
import scala.util.Try
import scala.util.control.NonFatal
import scalaz.{Divide => _, _}
import Scalaz._
import dahu.expr.Bind

import scala.collection.mutable



object Algebras {


  implicit def traverse: Traverse[ResultF] = new Traverse[ResultF] {
    override def traverseImpl[G[_], A, B](fa: ResultF[A])(f: (A) => G[B])(implicit G: Applicative[G]) = fa match {
      case InputF(name, typ) => G.point(InputF(name, typ))
      case CstF(value, typ) => G.point(CstF(value, typ))
      case ComputationF(fun, args, typ) =>
        ^^(G.point(fun), args.traverse(f), G.point(typ))(ComputationF(_, _, _): ResultF[B])
    }
  }

  val coalgebra: Coalgebra[ResultF, Expr[_]] = {
    case x @ Input(name) => InputF(name, x.typ)
    case x @ Cst(value) => CstF(Val(value), x.typ)
    case x @ Computation1(fun, arg) => ComputationF(fun, List(arg), x.typ)
    case x @ Computation2(fun, arg1, arg2) => ComputationF(fun, List(arg1, arg2), x.typ)
    case x @ Computation3(fun, a1, a2, a3) => ComputationF(fun, List(a1, a2, a3), x.typ)
  }

  val printAlgebra: Algebra[ResultF, String] = {
    case InputF(v, _)      => v
    case CstF(v, _) => v.v.toString
    case ComputationF(f, args, _) => f.name + args.mkString("(", ",", ")")
  }

  sealed abstract class EvalError
  final case class MissingVar(name: String)
  final case class ExceptionThrown(e: Throwable)

  def attempt[T](e: => T): ValidationNel[Throwable, T] = {
    try {
      Success(e)
    } catch {
      case NonFatal(e) => Failure(NonEmptyList(e))
    }
  }

  type TryEval[T] = ValidationNel[Throwable, T]

  def evalAlgebra(inputs: Map[String, Any]): Algebra[ResultF, TryEval[Top]] = {
    case InputF(v, _) => attempt { Val(inputs(v)) }
    case CstF(v, _) => attempt { v }
    case ComputationF(f, args, _) =>  args.sequenceU match {
      case Success(as) => attempt { Val(f.compute(as.map(_.v))) }
      case Failure(x) => Failure(x)
    }

  }
  val rec = Recursive.fromCoalgebra(coalgebra)

  def liftFix(in: Expr[Any]): Fix[ResultF] =
    in.ana[Fix[ResultF]].apply(coalgebra)


  def evaluate[T](prg: Expr[T], inputs: Seq[Bind[_]]): TryEval[T] = {
    val inputMap: Map[String, Any] = inputs.map {
      case Bind(variable, value) => variable.name -> value
    }.toMap
    rec.cata(prg)(evalAlgebra(inputMap)).asInstanceOf[TryEval[Val[T]]].map(_.v)
  }

  def pprint(prg: Expr[_]): String =
    rec.cata(prg)(printAlgebra)



  type Validated[T] = Option[ResultF[T]]

  type Result = Fix[ResultF]
  type Typed = Cofree[ResultF, TypeAlias.TT]
  type ValResult = Fix[Validated]

  // ResultF[Type]

  def extractType: Algebra[ResultF, TT] = _.typ

  type Key = Int
  type CodeStore = Map[Key, ResultF[Key]]

  final case class AST(head: Key, code: CodeStore) {
    override def toString: String = s"AST: $head\n" + code.mkString("  ", "\n  ", "")
  }

  def encode(in: Expr[Any]): AST = {
    val store = mutable.LinkedHashMap[ResultF[Key], Key]()
    val alg: Algebra[ResultF, Key] = e => {
      store.getOrElseUpdate(e, store.size)
    }

    val head = in.hylo(alg, coalgebra)
    val code = store.toList.map(_.swap).toMap
    AST(head, code)
  }


}
