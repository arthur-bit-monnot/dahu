package dahu.problem

import dahu.constraints.domains.{IntDomain, IntervalDomain}
import dahu.constraints._
import dahu.model.types._
import dahu.constraints.domains._
import dahu.maps.{ArrayMap, IMapBuilder, SubInt, SubSubInt}
import dahu.model.functions.Fun
import dahu.model.ir._
import dahu.utils.Errors._

object IntProblem {

  type Var = Int
  type Val = Int

  type Vars = Array[Var]
  type Vals = Array[Val]

  //  type Func = Vals => Val
  final case class Func(eval: Vals => Val, fw: ForwardPropagator, bw: BackwardPropagator)

  /** Computation is a function applied to some variables. */
  final case class Comp(f: Func, params: Vars)

  final case class Expr(domain: IntDomain, comp: Option[Comp], typ: TagIsoInt[_])
}
import IntProblem._

abstract class IntCSP[K <: SubInt] extends Problem[K, Int] {
  override def dom: K => IntDomain
  def exprs: K => Option[Comp]

  def getSolver: CSP[K]
}
object IntCSP {
  type Key[T <: SubInt] = SubSubInt[T, this.type]

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
        Some(Expr(domainOfType(typ), None, typ))
      case CstF(value, typ: TagIsoInt[_]) =>
        Some(Expr(SingletonDomain(typ.toIntUnsafe(value)), None, typ))
      case ComputationF(f, args, typ: TagIsoInt[_]) =>
        val expr: Option[Comp] = asIntFunction(f).map(fi => Comp(fi, args.toArray))
        Some(Expr(domainOfType(typ), expr, typ))
      case _ =>
        unexpected(s"${coalgebra(id)} is not supported.")
    }
  }

  def intSubProblem(ast: AST[_]): IntCSP[Key[ast.ID]] = {
    type K = Key[ast.ID]
    val factory = new IMapBuilder[Expr]

    ast.tree.domain
      .toIterable()
      .map(i => IntCSP.translate(i, ast.tree.asFunction).map((i, _)))
      .foreach {
        case Some((i, expr)) =>
          val e =
            if(i == ast.root)
              expr.copy(domain = True) // todo: should be set externally
            else
              expr

          factory += (i, e)
        case _ => // not representable or not in candidate set, ignore
      }
//    val externalInputs: Set[ast.ID] = {
//      factory.currentKeys
//        .toScalaSet()
//        .flatMap((v: Int) =>
//          ast.tree.get(v) match {
//            case Some(ComputationF(_, args, _)) => args.toSet
//            case Some(_)                        => Set[ast.ID]()
//            case _                              => unexpected
//        })
//    }
//    for(v <- externalInputs) {
//      assert(!factory.contains(v))
//      val expr = ast.tree.get(v).map(_.typ) match {
//        case Some(t: TagIsoInt[_]) => Expr(IntCSP.domainOfType(t), None, t)
//        case Some(_) =>
//          unexpected(
//            "Some computation in IntCSP depends on an expression whose type is not isomorphic to Int.")
//        case None => unexpected
//      }
//      factory += (v, expr)
//    }
    new IntCSP[K] {
      private val pb: ArrayMap.Aux[K, Expr] =
        factory.toImmutableArray.castKey[K]
      override def dom: K => IntDomain = pb(_).domain

      override def exprs: K => Option[Comp] = pb(_).comp

      override def vars: Array[K] = pb.domain.toArray

      override def getSolver: CSP[K] =
        new CSP(pb)
    }
  }
}
