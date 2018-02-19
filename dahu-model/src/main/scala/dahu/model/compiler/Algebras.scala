package dahu.model.compiler

import dahu.model.input._
import dahu.model.ir._
import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

import scala.util.control.NonFatal
import scalaz.{Divide => _, _}
import Scalaz._
import matryoshka.patterns.EnvT

import scala.collection.mutable
import dahu.model.types.Value

object Algebras {

  implicit def traverse: Traverse[ExprF] = new Traverse[ExprF] {
    override def traverseImpl[G[_], A, B](fa: ExprF[A])(f: (A) => G[B])(
        implicit G: Applicative[G]) = fa match {
      case InputF(name, typ) => G.point(InputF(name, typ))
      case CstF(value, typ)  => G.point(CstF(value, typ))
      case ComputationF(fun, args, typ) =>
        ^^(G.point(fun), args.toList.traverse(f), G.point(typ))(ComputationF(_, _, _): ExprF[B])
      case ProductF(members, typ) =>
        ^(members.toList.traverse(f), G.point(typ))(ProductF(_, _))
    }
  }

  val coalgebra: Coalgebra[ExprF, Expr[_]] = {
    case x @ Input(name)    => InputF(name, x.typ)
    case x @ Cst(value)     => CstF(Value(value), x.typ)
    case x: Computation[_]  => ComputationF(x.f, x.args, x.typ)
    case x @ Product(value) => ???
    //      val c = x.asComputation
    //      ComputationF(c.f, c.args, c.typ)
  }

  val printAlgebra: Algebra[ExprF, String] = {
    case InputF(v, _)             => v
    case CstF(v, _)               => v.toString
    case ComputationF(f, args, _) => f.name + args.mkString("(", ",", ")")
    case ProductF(members, _)     => members.mkString("(", ", ", ")")
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

  def evalAlgebra(inputs: Map[String, Value]): Algebra[ExprF, TryEval[Value]] = {
    case InputF(v, _) => attempt { inputs(v) }
    case CstF(v, _)   => attempt { v }
    case ComputationF(f, args, _) =>
      args.toList.sequenceU match {
        case Success(as) => attempt { Value(f.compute(as)) }
        case Failure(x)  => Failure(x)
      }
    case ProductF(members, _) => members.toList.sequenceU.map(Value(_))
  }
  val rec = Recursive.fromCoalgebra(coalgebra)

  def liftFix(in: Expr[Any]): Fix[ExprF] =
    in.ana[Fix[ExprF]].apply(coalgebra)

//  def evaluate[T](prg: Expr[T], inputs: Seq[Bind[_]]): TryEval[T] = {
//    val inputMap: Map[String, Value] = inputs
//      .map {
//        case Bind(variable, value) => variable.name -> value
//      }
//      .toMap
//      .mapValues(Value(_))
//    rec.cata(prg)(evalAlgebra(inputMap)).asInstanceOf[TryEval[T]]
//  }

  def pprint(prg: Expr[_]): String =
    rec.cata(prg)(printAlgebra)

  def pprint[T](coalgebra: Coalgebra[ExprF, T], expr: T): String = {
    // todo: use zygo directly
    val rec = Recursive.fromCoalgebra(coalgebra)
    rec.cata(expr)(printAlgebra)
  }

  def encodeAsPair(in: Expr[Any]): (Int, Vector[ExprF[Int]]) = {
    val store = mutable.LinkedHashMap[ExprF[Int], Int]()
    val alg: Algebra[ExprF, Int] = e => {
      store.getOrElseUpdate(e, store.size)
    }

    val head = in.hylo(alg, coalgebra)
    val code = store.toVector.sortBy(_._2).map(_._1)
    assert(store(code(0)) == 0)
    assert(store(code(code.size - 1)) == code.size - 1)

    (head, code)
  }

  // TODO: reintroduce after clean up of ASDAG
//  def transpile[T](t: T, coalgebra: Coalgebra[ExprF, T]): ASDAG[T] = {
//
//    import scala.collection.mutable
//    val store    = mutable.LinkedHashMap[ExprF[Int], Int]()
//    val astStore = mutable.LinkedHashMap[Int, mutable.ArrayBuffer[T]]()
//    val alg: Algebra[EnvT[T, ExprF, ?], Int] = {
//      case EnvT((x, e)) =>
//        val i = store.getOrElseUpdate(e, store.size)
//        astStore.getOrElseUpdate(i, mutable.ArrayBuffer()) += x
//        i
//    }
//    // this is only used to force traversal and populate the hash maps
//    val rootExprID: Int = t.hylo(alg, matryoshka.attributeCoalgebra(coalgebra))
//
//    val reverseAstStore           = astStore.flatMap(kp => kp._2.map((_, kp._1))).toMap
//    val reverseStore              = store.map(_.swap).toMap
//    val forward: T => Option[Int] = x => reverseAstStore.get(x)
//    val expr: Int => ExprF[Int]   = reverseStore(_)
//    import dahu.arrows._
//    new ASDAG[T] {
//      private val exprAlgebraData: Array[Expr] =
//        store.toArray.sortBy(_._2).map(e => ExprId.fromIntF(e._1))
//      private val compilationAlgebra: Map[T, ExprId] = reverseAstStore.asInstanceOf[Map[T, ExprId]]
//
//      override def coalgebra: ExprId ==> Expr = Arrow.lift(x => exprAlgebraData(x))
//
//      override def root: ExprId = ExprId.fromInt(rootExprID)
//
//      override def ids: OpaqueIntSubset[ExprId] = new OpaqueIntSubset[ExprId] {
//        override def wrap(i: Int): ExprId                 = ExprId.fromInt(i)
//        override def unwrap(a: ExprId): Int               = ExprId.toInt(a)
//        override def subst[F[_]](fi: F[Int]): F[ExprId]   = ExprId.fromIntF(fi)
//        override def unsubst[F[_]](fa: F[ExprId]): F[Int] = ExprId.toIntF(fa)
//
//        override val enumerate: Array[ExprId] = subst(exprAlgebraData.indices.toArray)
//      }
//
//      override def compiledForm: T ==> Option[ExprId] = Arrow.lift(compilationAlgebra.get)
//    }
//
//  }

}
