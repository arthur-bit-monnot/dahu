package dahu.model.problem

import cats.implicits._
import dahu.graphs.transformations.ManualTransformation
import dahu.model.input.Lambda
import dahu.model.ir._
import dahu.model.types._
import dahu.utils._

abstract class PartialEval extends ManualTransformation[ExprF, ExprF] {

  def trans[I <: Int, J <: Int](ctx: ManualTransformation.Context[ExprF, ExprF, I, J]): I => J = {
    val ev = new PartialEval.Impl[I, J](ctx)
    (i: I) =>
      ev.peval(Map())(i)
  }

}

object PartialEval {

  final class Impl[I <: Int, J <: Int](ctx: ManualTransformation.Context[ExprF, ExprF, I, J]) {

    import ctx._

    def known(l: Seq[J]): Option[List[Value]] =
      l.toList
        .map(nget)
        .map {
          case CstF(v, _) => Some(v)
          case _          => None
        }
        .sequence

    def peval(e: Map[Lambda.LambdaIdent, J])(i: I): J = oget(i) match {
      case ComputationF(f, argsExprs, tpe) =>
        val args = argsExprs.map(peval(e)(_))
        known(args.toList) match {
          case Some(values) => nrec(CstF(Value(f.compute(Vec.fromSeq(values))), f.outType))
          case None =>
            nrec(ComputationF(f, args, tpe))
        }
      case ApplyF(lbdI, argI, tpe) =>
        val arg = peval(e)(argI)
        oget(lbdI) match {
          case LambdaF(in, tree, ident, _) =>
            peval(e.updated(ident, arg))(tree)
          case _ =>
            nrec(ApplyF(peval(e)(lbdI), arg, tpe))
        }
      case LambdaParamF(id, tpe) =>
        e.get(id) match {
          case Some(j) => j
          case None    => nrec(LambdaParamF(id, tpe))
        }
      case x =>
        nrec(x.smap(peval(e)(_)))
    }
  }
}
