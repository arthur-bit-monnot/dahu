package dahu.problem

import dahu.constraints.domains.{IntDomain, IntervalDomain}
import dahu.constraints._
import dahu.expr.Fun
import dahu.expr.types.TagIsoInt
import dahu.recursion._
import dahu.recursion.Types._
import dahu.constraints.domains._
import dahu.structures.{ArrayIntFunc, IntFuncBuilder, MutableMapIntFunc}
import dahu.utils.Errors._

import scala.collection.mutable

object IntProblem {

  type Var = ExprId
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

abstract class IntCSP[T] extends Problem[T,Int] {
  override def dom: KI[T] => IntDomain
  def exprs: KI[T] => Option[Comp]

  def getSolver: CSP[T]
}
object IntCSP {
  def domainOfType(typ: TagIsoInt[_]): IntervalDomain = IntervalDomain(typ.min, typ.max)

  def translate(id: Var, coalgebra: Var => ExprF[Var]): Option[IntProblem.Expr] = {
    import dahu.expr.labels.Labels.Value

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
        None

    }
  }

  def intSubProblem[T0](asg: ASDAG[_])(candidates: ExprId => Boolean): IntCSP[T0] = {
    val factory = new IntFuncBuilder[(IntDomain, Option[Comp])]
//    val xxx = new MutableMapIntFunc[T0, (IntDomain, Option[Comp])]
    asg.ids.enumerate
      .map(i => IntCSP.translate(i, asg.coalgebra.asScalaFunction).map((i, _)))
      .foreach {
        case Some((i, expr)) if candidates(i) =>
          val dom =
            if(i == asg.root)
              True // todo: should be set externally
            else
              expr.domain

          // only treat as computation if it is representable and is in the candidate set
          val e = expr.comp
            .flatMap(c => if(candidates(i)) Some(c) else None)
          factory += (i, (dom, e))
        case _ => // not representable or not in candidate set, ignore
      }
    val externalInputs: Set[Var] = {
      factory.currentKeys
        .toScalaSet()
        .flatMap((v: Int) =>
          asg.coalgebra(ExprId.fromInt(v)) match {
            case ComputationF(_, args, _) => args.toSet
            case _                        => Set[Var]()
          })
        .filterNot(candidates)
    }
    for(v <- externalInputs) {
      assert(!factory.contains(v))
      val dom = asg.coalgebra(v).typ match {
        case t: TagIsoInt[_] => IntCSP.domainOfType(t)
        case _ =>
          unexpected(
            "Some computation in IntCSP depends on an expression whose type is not isomorphic to Int.")
      }
      factory += (v, (dom, None))
    }
    new IntCSP[T0] {
      private val pb: ArrayIntFunc[T0, (IntDomain, Option[Comp])] = factory.toImmutableArray
      override def dom: KI[T0] => IntDomain = pb(_)._1

      override def exprs: KI[T0] => Option[Comp] = pb(_)._2

      override def vars: Array[KI[T0]] = pb.domain.toArray

      override def getSolver: CSP[T0] =
        new CSP(pb)
    }
  }
}
