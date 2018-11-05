package dahu.model.problem

import cats.implicits._
import dahu.graphs.transformations.ManualTransformation
import dahu.model.input.Lambda
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.products.{Field, FieldAccess, FieldAccessAny, GetField}
import dahu.model.types._
import dahu.utils._

import scala.collection.mutable.ArrayBuffer

object PartialEval extends ManualTransformation[ExprF, ExprF] {

  def trans[I <: Int, J <: Int](ctx: ManualTransformation.Context[ExprF, ExprF, I, J]): I => J = {
    val ev = new PartialEval.Impl[I, J](ctx)
    (i: I) =>
      ev.peval(Map())(i)
  }

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

      case ComputationF(f: GetField, Vec(arg), _) =>
        val newArg = peval(e)(arg)
        nget(newArg) match {
          case ProductF(members, tpe) =>
            tpe.fields.toSeq.find(_.name == f.fieldName) match {
              case Some(Field(_, _, pos)) =>
                members(pos)
              case None =>
                nrec(ComputationF(f, Vec(newArg), f.outType))
            }
          case x =>
//            println(x)
            nrec(ComputationF(f, Vec(newArg), f.outType))
        }
      case ComputationF(f: FieldAccessAny, Vec(arg), tpe) =>
        nget(peval(e)(arg)) match {
          case ProductF(members, _) => members(f.fieldPosition)
          case _                    => nrec(ComputationF(f, Vec(peval(e)(arg)), tpe))

        }
      case ComputationF(bool.And, args, tpe) =>
        val conjuncts = ArrayBuffer[J]()
        for(a <- args) {
          nget(peval(e)(a)) match {
            case CstF(true, _)  =>
            case CstF(false, _) => return nrec(bool.FalseF)
            case x              => conjuncts += nrec(x)
          }
        }
        if(conjuncts.isEmpty)
          nrec(bool.TrueF)
        else if(conjuncts.size == 1)
          conjuncts(0)
        else
          nrec(ComputationF(bool.And, conjuncts.toVec, Tag.ofBoolean))

      case ComputationF(bool.Or, args, tpe) =>
        val conjuncts = ArrayBuffer[J]()
        for(a <- args) {
          nget(peval(e)(a)) match {
            case CstF(false, _) =>
            case CstF(true, _)  => return nrec(bool.TrueF)
            case x              => conjuncts += nrec(x)
          }
        }
        if(conjuncts.isEmpty)
          nrec(bool.FalseF)
        else if(conjuncts.size == 1)
          conjuncts(0)
        else
          nrec(ComputationF(bool.Or, conjuncts.toVec, Tag.ofBoolean))

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
