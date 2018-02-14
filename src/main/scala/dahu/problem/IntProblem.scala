package dahu.problem

import dahu.constraints.domains.{IntDomain, IntervalDomain}
import dahu.constraints._
import dahu.expr.Fun
import dahu.expr.types.TagIsoInt
import dahu.recursion._
import dahu.recursion.Types._
import dahu.constraints.domains._
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

abstract class IntCSP extends Problem[Int] {
  def vars: Array[Var]
  def dom: Var => Option[IntDomain]
  def exprs: Var => Option[Comp]

  def getSolver: CSP[_]
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

  def intSubProblem(asg: ASDAG[_])(candidates: ExprId => Boolean): IntCSP = {
    val xxx = new MutableMapIntFunc[(IntDomain, Option[Comp])] { type T = Any }
//    val variables = mutable.ArrayBuffer[Var]()
//    val domains = new Array[IntDomain](asg.ids.last.value + 1)
//    val expressions = new Array[Comp](asg.ids.last.value + 1)
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
          xxx.extend(i, (dom, e))
        case _ => // not representable or not in candidate set, ignore
      }
    val externalInputs: Set[Var] = {
      xxx.domain
        .flatMap((v: xxx.Key) =>
          asg.coalgebra(ExprId.fromInt(v)) match {
            case ComputationF(_, args, _) => args.toSet
            case _                        => Set[Var]()
        })
        .filterNot(candidates)
    }
    for(v <- externalInputs) {
      assert(!xxx.isInDomain(v))
      val dom = asg.coalgebra(v).typ match {
        case t: TagIsoInt[_] => IntCSP.domainOfType(t)
        case _ =>
          unexpected(
            "Some computation in IntCSP depends on an expression whose type is not isomorphic to Int.")
      }
      xxx.extend(v, (dom, None))
    }
    new IntCSP {
      private val pb: IntFunc.Aux[xxx.T, (IntDomain, Option[Comp])] = xxx
      override def dom: Var => Option[IntDomain] = pb.get(_).map(_._1)

      override def exprs: Var => Option[Comp] = pb.get(_).flatMap(_._2)

      override def vars: Array[Var] = ExprId.fromIntF(pb.domain.toArray)

      override def hasVar(v: ExprId): Boolean = pb.isInDomain(v)

      override def getSolver: CSP[_] =
        new CSP[pb.T](pb)
    }
  }
}
