package dahu.planning.planner.hcsp

import dahu.model.input.{Cst, Expr, Input, LocalIdent, Scope}
import dahu.model.input.dsl._
import dahu.model.types.Tag
import dahu.planning.model.common.LocalVar
import dahu.planning.model.common.operators.BinaryOperator
import dahu.planning.model.common.{Cst => _, _}
import dahu.planning.model.{common, core}
import dahu.planning.planner.chronicles.{Fluent, FluentF, IntervalF, SCondTokF}

import scala.collection.mutable.{ArrayBuffer => Buff}
import scala.collection.mutable.{Map => MMap}

case class CExpr[+A](e: Expr[A], ctx: Scope)

abstract class CSP {
  val vars: MMap[Any, Expr[Literal]] = MMap()
  val constraints: Buff[CExpr[Boolean]] = Buff()
  val subs: Buff[CSP] = Buff()
  val exports: Buff[CExpr[Any]] = Buff()

  val ctx: ProblemContext
  def scope: Scope
  def temporalOrigin: Expr[Int]
  def temporalHorizon: Expr[Int]
  def parent: Option[CSP]
  val resolver: VariableResolver = new VariableResolver {
    override def getArg(a: Arg): Expr[Literal] = getVar(a).get
    override def getLocalVar(v: LocalVar): Expr[Literal] = getVar(v).get
  }

  def present: Expr[Boolean] = scope.present

  final def getVar(id: Any): Option[Expr[Literal]] = {
    vars.get(id).orElse(parent.flatMap(_.getVar(id)))
  }

  final def addConstraint(c: Expr[Boolean]): Unit = {
    constraints += CExpr(c, scope)
  }
  final def newInput[T](id: Any, typTag: Tag[T]): Expr[T] = Input[T](id, scope)(typTag)
  final def newInputSubjectTo[T](id: Any, typTag: Tag[T])(
      constraint: Expr[T] => Expr[Boolean]): Expr[T] = {
    val in = newInput(id, typTag)
    addConstraint(constraint(in))
    in
  }
  final def recordVar(id: Any, e: Expr[Literal]): Unit = {
    assert(getVar(id).isEmpty)
    vars.update(id, e)
  }

  final def getTypedVar[T: Tag](id: Any): Option[Expr[T]] = {
    getVar(id) match {
      case Some(v) =>
        assert(v.typ == Tag[T], "queried a variable with the wrong type")
        Some(v).asInstanceOf[Some[Expr[T]]]
      case None => None
    }
  }

  final def addSub(name: String): CSP = {
    val subPresent = newInput[Boolean](scope.subId(name + "??"), Tag.ofBoolean)
    val subScope = scope.subScope(name, subPresent)
    val subCSP = new SubCSP(this, subScope)
    subs += subCSP
    addConstraint(subPresent ==> present)
    subCSP
  }

  def addExport(e: Expr[Any]): CExpr[Any] = {
    val ce = CExpr(e, scope)
    exports += ce
    ce
  }

  // ------------------ extension methods for planner -----------------------

  def addVar(lv: LocalVar): Expr[Literal] = lv match {
    case lv @ LocalVar(id, tpe) if tpe.isSubtypeOf(ctx.predef.Time) =>
      assert(tpe == ctx.predef.Time)
      val e: Expr[Int] =
        if(lv == ctx.predef.Start)
          ctx.temporalOrigin
        else if(lv == ctx.predef.End)
          ctx.temporalHorizon
        else {
          newInputSubjectTo[Int]((lv, "int-version"), Tag.ofInt)(tp =>
            ctx.temporalOrigin <= tp && tp <= ctx.temporalHorizon)
        }
      val res = ctx.intBox(ctx.intTag, e)
      recordVar(lv, res)
      res
    case lv @ LocalVar(_, tpe) if tpe.isSubtypeOf(Type.Integers) =>
      assert(tpe == Type.Integers)
      val res = newInput[Literal](lv, ctx.intTag)
      recordVar(lv, res)
      res
    case lv @ LocalVar(_, tpe) =>
      assert(!tpe.isSubtypeOf(Type.Reals))
      val res = newInput[Literal](lv, ctx.specializedTags(tpe))
      recordVar(lv, res)
      res
  }

  import ctx._
  implicit private def _resolver = resolver

  def encode(orig: core.Fluent): Expr[Fluent] =
    FluentF.ofExpr(orig.template, orig.params.map(ctx.encode(_)))

  def encode(orig: common.Constant)(implicit argRewrite: Arg => Expr[Literal]): Expr[Fluent] =
    FluentF.ofExpr(orig.template, orig.params.map(ctx.encode(_)))

  def encode(orig: common.Interval[common.Expr])(
      implicit argRewrite: Arg => Expr[Literal]): IntervalF[Expr] =
    orig.map(ctx.encodeAsInt(_)) match {
      case ClosedInterval(s, e)    => IntervalF.ofExprUnwrapped(s, e)
      case LeftOpenInterval(s, e)  => IntervalF.ofExprUnwrapped(s - (-1), e)
      case RightOpenInterval(s, e) => IntervalF.ofExprUnwrapped(s, e - 1)
      case OpenInterval(s, e)      => IntervalF.ofExprUnwrapped(s - (-1), e - 1)
    }

  def encode(e: common.Expr)(implicit argRewrite: Arg => Expr[Literal]): Expr[Literal] =
    e match {
      case term: common.Term => ctx.encode(term)
      case Op2(operators.Eq, v, cst: common.Constant) =>
        ctx.boolBox(SCondTokF.ofExpr(encode(cst), encode(v)))
      case Op2(op, left, right) =>
        applyOperator(op, encode(left), encode(right))
      case _ => ???
    }

  def extendWith(e: core.InActionBlock)(implicit
                                        cnt: Counter): Unit =
    e match {
//      case _: core.LocalVarDeclaration => this
//      case _: core.ArgDeclaration      => this
//
//      case core.TimedAssignmentAssertion(itv, fluent, value) => ???
//        val changeItv = encode(itv)
//        val persistenceEnd = anonymousTp().alwaysSubjectTo(changeItv.end <= _)
//        val token =
//          EffTokF.ofExpr(changeItv.start,
//                         changeItv.end,
//                         persistenceEnd,
//                         encode(fluent),
//                         ctx.encode(value))
//        copy(effects = token :: effects)
//
//      case core.TimedEqualAssertion(itv, f, v) =>
//        val interval = encode(itv)
//        val token = CondTokF.ofExpr(interval.start, interval.end, encode(f), ctx.encode(v))
//        copy(conditions = token :: conditions)
//
//      case core.TimedTransitionAssertion(ClosedInterval(s, e), f, v1, v2) =>
//        val start = encodeAsInt(s)
//        val cond =
//          CondTokF.ofExpr(start, start, encode(f), ctx.encode(v1))
//        val changeItv = encode(LeftOpenInterval(s, e))
//        val persistenceEnd = anonymousTp().alwaysSubjectTo(changeItv.end <= _)
//        val eff =
//          EffTokF.ofExpr(changeItv.start, changeItv.end, persistenceEnd, encode(f), ctx.encode(v2))
//        copy(
//          conditions = cond :: conditions,
//          effects = eff :: effects
//        )
//
//      case core.StaticAssignmentAssertion(lhs, rhs) =>
//        val eff = SEffTokF.ofExpr(encode(lhs), ctx.encode(rhs))
//        copy(
//          staticEffects = eff :: staticEffects
//        )
//      case core.StaticBooleanAssertion(e) =>
//        val c = ctx.boolUnbox(encode(e))
//        copy(
//          constraints = c :: constraints
//        )
      case _ => ???
    }
  def extendWithActions(template: core.ActionTemplate, howMany: Int): Unit = {
    ???
  }
}

final class RootCSP(val ctx: ProblemContext) extends CSP {

  override def scope: Scope = Scope.root
  override def parent: Option[CSP] = None

  override val temporalOrigin: Expr[Int] = Cst(0)
  override val temporalHorizon: Expr[Int] = newInput("__END__", Tag[Int])
  addConstraint(temporalOrigin <= temporalHorizon)
}

final class SubCSP(val father: CSP, val scope: Scope) extends CSP {
  override val ctx: ProblemContext = father.ctx
  override def temporalOrigin: Expr[Int] = father.temporalOrigin
  override def temporalHorizon: Expr[Int] = father.temporalHorizon
  override def parent: Option[CSP] = Some(father)
}
