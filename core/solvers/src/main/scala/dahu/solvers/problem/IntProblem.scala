package dahu.solvers.problem

import dahu.utils._
import dahu.constraints.domains.{IntDomain, IntervalDomain}
import dahu.constraints._
import dahu.model.types._
import dahu.constraints.domains._
import dahu.utils._
import dahu.model.functions.{Fun, FunAny}
import dahu.model.ir._
import dahu.utils.errors._

object IntProblem {

  type Var = Int
  type Val = Int

  type Vars = Vec[Var]
  type Vals = Vec[Val]

  final case class Func(eval: Vals => Val, fw: ForwardPropagator, bw: BackwardPropagator)

  /** Computation is a function applied to some variables. */
  final case class Comp(f: Func, params: Vars)

  final case class Expr(domain: IntDomain, comp: Option[Comp], typ: TagIsoInt[_])
}
import IntProblem._

abstract class IntCSP[K <: SubInt] extends Problem[K, Int] {
  def toSatisfy: Set[K]

  override def dom: K => IntDomain
  def exprs: K => Option[Comp]

  def getSolver: CSP[K]
}
object IntCSP {
  type Key[T <: SubInt] = SubSubInt[T, this.type]

  def domainOfType(typ: TagIsoInt[_]): IntervalDomain = IntervalDomain(typ.min, typ.max)

  def translate[T <: SubInt](e: Total[T]): Option[IntProblem.Expr] = {
    def asIntFunction(f: FunAny): Option[Func] = {
      IntCompatibleFunc
        .compat(f)
        .map(icl =>
          (args: Vals) => {
            icl.outToInt(Value(f.compute(icl.argsFromInt(args))))
        })
        .map(i2iFunc => Func(i2iFunc, Propagator.forward(f), Propagator.backward(f)))
    }

    e match {
      case InputF(_, typ: TagIsoInt[_]) =>
        Some(Expr(domainOfType(typ), None, typ))
      case CstF(value, typ: TagIsoInt[_]) =>
        Some(Expr(SingletonDomain(typ.toIntUnsafe(value)), None, typ))
      case ComputationF(f, args, typ: TagIsoInt[_]) =>
        val expr: Option[Comp] = asIntFunction(f).map(fi => Comp(fi, args.upcast))
        Some(Expr(domainOfType(typ), expr, typ))
      case _ =>
        unexpected(s"$e is not supported.")
    }
  }
//  def intProblem[X](ast: RootedLazyTree[X, Total]): IntCSP[X] = {
////    type K = IntCSP.Key[ast.ID]
//
//    val factory = new IMapBuilder[Expr]
//
////    val nodes = ast.nodes
////      .map { case (a, fa) => IntCSP.translate(fa).map((a, _)) }
////      .foreach {
////        case Some((i, expr)) => factory += (i, expr)
////        case _ => // not representable ??? // TODO: doule check that we don't want to raise an error.
////      }
//
////    ast.algebra.domain
////      .toIterable()
////      .map(i => IntCSP.translate(i, ast.algebra.asFunction).map((i, _)))
////      .foreach {
////        case Some((i, expr)) =>
////          factory += (i, expr)
////        case _ => // not representable or not in candidate set, ignore
////      }
////
////    val conjunction = Set(ast.root.asInstanceOf[K])
////    new IntCSP[K] {
////      private val pb: ArrayMap.Aux[K, Expr] =
////        factory.toImmutableArray.castKey[K]
////
////      override def toSatisfy: Set[K] = conjunction
////
////      override def dom: K => IntDomain = pb(_).domain
////
////      override def exprs: K => Option[Comp] = pb(_).comp
////
////      override def vars: Array[K] = pb.domain.toArray
////
////      override def getSolver: CSP[K] =
////        new CSP(pb, toSatisfy)
////    }
//      ???
//  }
}
