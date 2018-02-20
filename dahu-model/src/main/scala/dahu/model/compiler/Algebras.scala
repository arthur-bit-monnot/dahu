package dahu.model.compiler

import cats.free.Cofree
import cats.{Applicative, Functor, Traverse}
import cats.implicits._
import dahu.arrows.ArrayIntFunc
import dahu.arrows.ArrayIntFunc.Aux
import dahu.model.input._
import dahu.model.ir._

import scala.util.control.NonFatal
import scala.collection.mutable
import dahu.model.types.{Type, Value}
import dahu.recursion._

object Algebras {

  implicit def functor: Functor[ExprF] = new Functor[ExprF] {
    override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case x: InputF[A] => x
      case x: CstF[A] => x
      case ComputationF(fun, args, typ) => ComputationF(fun, args.map(f), typ)
      case ProductF(members, typ) => ProductF(members.map(f), typ)
    }
  }

//  implicit def traverse: Traverse[ExprF] = new Traverse[ExprF] {
//    override def traverseImpl[G[_], A, B](fa: ExprF[A])(f: (A) => G[B])(
//        implicit G: Applicative[G]) = fa match {
//      case InputF(name, typ) => G.point(InputF(name, typ))
//      case CstF(value, typ)  => G.point(CstF(value, typ))
//      case ComputationF(fun, args, typ) =>   ??? // TODO
////        ^^(G.point(fun), args.toList.traverse(f), G.point(typ))(ComputationF(_, _, _): ExprF[B])
//      case ProductF(members, typ) =>
//        G.ap2(G.point((x: List[B], y: Type) =>ProductF(x, y)))(members.toList.traverse(f), G.point(typ))
////        ^(members.toList.traverse(f), G.point(typ))(ProductF(_, _))
//    }
//  }

  val coalgebra: FCoalgebra[ExprF, Expr[_]] = {
    case x @ Input(name)    => InputF(name, x.typ)
    case x @ Cst(value)     => CstF(Value(value), x.typ)
    case x: Computation[_]  => ComputationF(x.f, x.args, x.typ)
    case x @ Product(value) => ???
    //      val c = x.asComputation
    //      ComputationF(c.f, c.args, c.typ)
  }

  val printAlgebra: FAlgebra[ExprF, String] = {
    case InputF(v, _)             => v
    case CstF(v, _)               => v.toString
    case ComputationF(f, args, _) => f.name + args.mkString("(", ",", ")")
    case ProductF(members, _)     => members.mkString("(", ", ", ")")
  }

  sealed abstract class EvalError
  final case class MissingVar(name: String)
  final case class ExceptionThrown(e: Throwable)

//  def attempt[T](e: => T): ValidationNel[Throwable, T] = {
//    try {
//      Success(e)
//    } catch {
//      case NonFatal(e) => Failure(NonEmptyList(e))
//    }
//  }
//
//  type TryEval[T] = ValidationNel[Throwable, T]

//  def evalAlgebra(inputs: Map[String, Value]): Algebra[ExprF, TryEval[Value]] = {
//    case InputF(v, _) => attempt { inputs(v) }
//    case CstF(v, _)   => attempt { v }
//    case ComputationF(f, args, _) =>
//      args.toList.sequenceU match {
//        case Success(as) => attempt { Value(f.compute(as)) }
//        case Failure(x)  => Failure(x)
//      }
//    case ProductF(members, _) => members.toList.sequenceU.map(Value(_))
//  }
//  val rec = Recursive.fromCoalgebra(coalgebra)
//
//  def liftFix(in: Expr[Any]): Fix[ExprF] =
//    in.ana[Fix[ExprF]].apply(coalgebra)

//  def evaluate[T](prg: Expr[T], inputs: Seq[Bind[_]]): TryEval[T] = {
//    val inputMap: Map[String, Value] = inputs
//      .map {
//        case Bind(variable, value) => variable.name -> value
//      }
//      .toMap
//      .mapValues(Value(_))
//    rec.cata(prg)(evalAlgebra(inputMap)).asInstanceOf[TryEval[T]]
//  }

  import dahu.recursion.Recursion._

  def pprint(prg: Expr[_]): String =
    hylo(coalgebra, printAlgebra)(prg)

  def pprint[T](coalgebra: FCoalgebra[ExprF, T], expr: T): String =
    hylo(coalgebra, printAlgebra)(expr)

//  def encodeAsPair(in: Expr[Any]): (Int, Vector[ExprF[Int]]) = {
//    val store = mutable.LinkedHashMap[ExprF[Int], Int]()
//    val alg: Algebra[ExprF, Int] = e => {
//      store.getOrElseUpdate(e, store.size)
//    }
//
//    val head = in.hylo(alg, coalgebra)
//    val code = store.toVector.sortBy(_._2).map(_._1)
//    assert(store(code(0)) == 0)
//    assert(store(code(code.size - 1)) == code.size - 1)
//
//    (head, code)
//  }

  def parse[T](e: Expr[T]): AST[Expr[_]] =
    parse(e, coalgebra)
  def parse[T](t: T, coalgebra: FCoalgebra[ExprF, T]): AST[T] = {

    import scala.collection.mutable
    val store = mutable.LinkedHashMap[ExprF[Int], Int]()
    val astStore = mutable.LinkedHashMap[Int, mutable.ArrayBuffer[T]]()
    val alg: CVAlgebra[ExprF, (Int, T)] = {
      case null => ???
    }
//    val alg: FAlgebra[EnvT[T, ExprF, ?], Int] = {
//      case EnvT((x, e)) =>
//        val i = store.getOrElseUpdate(e, store.size)
//        astStore.getOrElseUpdate(i, mutable.ArrayBuffer()) += x
//        i
//    }
    // this is only used to force traversal and populate the hash maps
    val rootExprID: Int = chrono(coalgebra.toCVCoalgebra, alg)(t)._1 // t.hylo(alg, matryoshka.attributeCoalgebra(coalgebra))

    val reverseAstStore = astStore.flatMap(kp => kp._2.map((_, kp._1))).toMap
    val reverseStore = store.map(_.swap).toMap
    val forward: T => Option[Int] = x => reverseAstStore.get(x)
    val expr: Int => ExprF[Int] = reverseStore(_)

    val tree: ArrayIntFunc[ExprF[Int]] = ArrayIntFunc.build(store.values, expr)
    val casted: ArrayIntFunc.Aux[tree.K, ExprF[tree.K]] = tree.map(_.asInstanceOf[ExprF[tree.K]])
    assert(casted.isInDomain(rootExprID))
    val root = rootExprID.asInstanceOf[tree.K]
    val fromInput = forward.asInstanceOf[T => Option[tree.K]]
    new ASTImpl(casted, root, fromInput)
  }

}
