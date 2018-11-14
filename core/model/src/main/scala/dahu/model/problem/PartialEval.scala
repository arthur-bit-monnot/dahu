package dahu.model.problem

import cats.implicits._
import dahu.graphs.transformations.ManualTransformation
import dahu.model.functions.{Fun1, Fun2, FunAny, FunAny2}
import dahu.model.input.Lambda.LambdaIdent
import dahu.model.input.{Lambda, TypedIdent}
import dahu.model.ir._
import dahu.model.math.bool
import dahu.model.products.{Field, FieldAccess, FieldAccessAny, GetField}
import dahu.model.types._
import dahu.utils._

import scala.collection.mutable.ArrayBuffer

/** Takes two parameters
  * first: expression to be replaced by the parameter of the lambda
  * second: tree to be made the body of the lambda
  * */
object ExtractInputToLambda extends FunAny2 {
  override def isMacro: Boolean = true
  override def compute(a: Any, b: Any): Any = ???
  override def name: String = "as-lambda"
  private def inType = Tag.unsafe.ofAny
  override def outType = LambdaTag.of(Tag.unsafe.ofAny, Tag.unsafe.ofAny)
  override def funType: LambdaTagAny = LambdaTag.of(Tag.unsafe.ofAny, inType, outType)

}

object PartialEval extends ManualTransformation[ExprF, ExprF] {

  def trans[I <: Int, J <: Int](ctx: ManualTransformation.Context[ExprF, ExprF, I, J]): I => J = {
    val ev = new PartialEval.Impl[I, J](ctx)
    (i: I) =>
      ev.peval(Env(Map()))(i)
  }

  private case class Env[I, J](e: Map[I, J]) {
//    def updated(id: Lambda.LambdaIdent, j: J) = Env(e.updated(id, j), e2)
//    def updated(id: TypedIdent, j: J) = Env(e, e2.updated(id, j))
//    def get(id: LambdaIdent) = e.get(id)
//    def get(id: TypedIdent) = e2.get(id)
    def addReplace(i: I, j: J) = Env(e.updated(i, j))
    def contains(i: I): Boolean = e.contains(i)
    def get(i: I): J = e(i)
  }

  private final class Impl[I <: Int, J <: Int](
      ctx: ManualTransformation.Context[ExprF, ExprF, I, J]) {

    import ctx._

    def known(l: Seq[J]): Option[List[Value]] =
      l.toList
        .map(nget)
        .map {
          case CstF(v, _) => Some(v)
          case _          => None
        }
        .sequence

    def peval(e: Env[I, J])(i: I): J = oget(i) match {

      case _ if e.contains(i) => e.get(i)

      case ComputationF(ExtractInputToLambda, Vec(param, expr), _) =>
        val id = LambdaIdent("x")
        val j = nrec(LambdaParamF(id, oget(param).typ))
        val substituted = peval(e.addReplace(param, j))(expr)
        nrec(LambdaF(j, substituted, id, ExtractInputToLambda.outType))

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
          case prod @ ProductF(members, prodType) =>
//            assert(f.prodTag == prodType, s"Expected ${f.prodTag} but got $prodType") // TODO, type equality is not working for optionalF
            assert(f.fieldPosition < members.length, s"$prod -- ${prod.typ}")
            members(f.fieldPosition)
          case _ => nrec(ComputationF(f, Vec(peval(e)(arg)), tpe))

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

      case ComputationF(f, argsExprs, tpe) if !f.isMacro =>
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
            peval(e.addReplace(in, arg))(tree)
          case CstF(fun: FunAny, tpe) if fun.arity.contains(1) =>
            nrec(ComputationF(fun, arg))
          case CstF(fun: FunAny, tpe) =>
            dahu.utils.debug.warning(
              s"Cannot partially evaluate intrinsic function $fun that expects more that one parameter")
            nrec(ApplyF(peval(e)(lbdI), arg, tpe)) // same as default case
          case _ =>
            nrec(ApplyF(peval(e)(lbdI), arg, tpe))
        }
//      case LambdaParamF(id, tpe) =>
//        e.get(id) match {
//          case Some(j) => j
//          case None    => nrec(LambdaParamF(id, tpe))
//        }
      case x =>
        nrec(x.smap(peval(e)(_)))
    }
  }
}
