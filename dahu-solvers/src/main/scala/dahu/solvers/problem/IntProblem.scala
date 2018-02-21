package dahu.problem

import dahu.constraints.domains.{IntDomain, IntervalDomain}
import dahu.constraints._
import dahu.model.types._
import dahu.constraints.domains._
import dahu.maps.{ArrayMap, IMapBuilder, SInt, SubInt}
import dahu.model.functions.Fun
import dahu.model.ir._
import dahu.utils.Errors._

import scala.collection.mutable

object IntProblem {

  type Var = Int
  type Val = Int

  type Vars = Array[Var]
  type Vals = Array[Val]

  //  type Func = Vals => Val
  case class Func(eval: Vals => Val, fw: ForwardPropagator, bw: BackwardPropagator)

  /** Computation is a function applied to some variables. */
  case class Comp(f: Func, params: Vars)

  case class Expr(domain: IntDomain, comp: Option[Comp])
}
import IntProblem._

abstract class IntCSP[K <: SubInt] extends Problem[K, Int] {
  override def dom: K => IntDomain
  def exprs: K => Option[Comp]

  def getSolver: CSP[K]
}
object IntCSP {
  def domainOfType(typ: TagIsoInt[_]): IntervalDomain = IntervalDomain(typ.min, typ.max)

  def translate[T <: SubInt](id: T, coalgebra: T => ExprF[T]): Option[IntProblem.Expr] = {
    def asIntFunction(f: Fun[_]): Option[Func] = {
      // todo: cache generated function
      IntCompatibleFunc
        .compat(f)
        .map(icl =>
          (args: Vals) => {
            icl.outToInt(Value(f.compute(icl.argsFromInt(args))))
        })
        .map(i2iFunc => Func(i2iFunc, Propagator.forward(f), Propagator.backward(f)))
    }

    coalgebra(id) match {
      case InputF(_, typ: TagIsoInt[_]) =>
        Some(Expr(domainOfType(typ), None))
      case CstF(value, typ: TagIsoInt[_]) =>
        Some(Expr(SingletonDomain(typ.toIntUnsafe(value)), None))
      case ComputationF(f, args, typ: TagIsoInt[_]) =>
        val expr: Option[Comp] = asIntFunction(f).map(fi => Comp(fi, args.toArray))
        Some(Expr(domainOfType(typ), expr))
      case _ =>
        unexpected(s"${coalgebra(id)} is not supported.")
        None

    }
  }

  def intSubProblem(ast: AST[_])(candidates: Var => Boolean): IntCSP[ast.ID] = {
    type T0 = ast.ID
    val factory = new IMapBuilder[(IntDomain, Option[Comp])]
//    val xxx = new MutableMapIntFunc[T0, (IntDomain, Option[Comp])]
    ast.tree.domain
      .toIterable()
      .map(i => IntCSP.translate(i, ast.tree.asFunction).map((i, _)))
      .foreach {
        case Some((i, expr)) if candidates(i) =>
          val dom =
            if(i == ast.root)
              True // todo: should be set externally
            else
              expr.domain

          // only treat as computation if it is representable and is in the candidate set
          val e = expr.comp
            .flatMap(c => if(candidates(i)) Some(c) else None)
          factory += (i, (dom, e))
        case _ => // not representable or not in candidate set, ignore
      }
    val externalInputs: Set[ast.ID] = {
      factory.currentKeys
        .toScalaSet()
        .flatMap((v: Int) =>
          ast.tree.get(v) match {
            case Some(ComputationF(_, args, _)) => args.toSet
            case Some(_)                        => Set[ast.ID]()
            case _                              => unexpected
        })
        .filterNot(candidates)
    }
    for(v <- externalInputs) {
      assert(!factory.contains(v))
      val dom = ast.tree.get(v).map(_.typ) match {
        case Some(t: TagIsoInt[_]) => IntCSP.domainOfType(t)
        case Some(_) =>
          unexpected(
            "Some computation in IntCSP depends on an expression whose type is not isomorphic to Int.")
        case None => unexpected
      }
      factory += (v, (dom, None))
    }
    new IntCSP[T0] {
      private val pb: ArrayMap.Aux[T0, (IntDomain, Option[Comp])] =
        factory.toImmutableArray.castKey[T0]
      override def dom: T0 => IntDomain = pb(_)._1

      override def exprs: T0 => Option[Comp] = pb(_)._2

      override def vars: Array[T0] = pb.domain.toArray

      override def getSolver: CSP[T0] =
        new CSP(pb)
    }
  }
}
