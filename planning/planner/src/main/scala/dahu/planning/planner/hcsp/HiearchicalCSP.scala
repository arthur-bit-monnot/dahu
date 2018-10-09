package dahu.planning.planner.hcsp

import dahu.model.input.{Cst, Expr, Input, LocalIdent, Scope}
import dahu.model.input.dsl._
import dahu.model.types.Tag
import dahu.planning.model.common.LocalVar
import dahu.planning.model.common.operators.BinaryOperator
import dahu.planning.model.common.{Cst => _, _}
import dahu.planning.model.transforms.ActionInstantiation
import dahu.planning.model.{common, core}
import dahu.planning.planner.chronicles
import dahu.planning.planner.chronicles.{Fluent, FluentF, IntervalF, SCondTokF}
import dahu.solvers.problem.{CExpr, Struct}
import dahu.utils.Vec

import scala.collection.mutable.{ArrayBuffer => Buff}
import scala.collection.mutable.{Map => MMap}
import scala.reflect.ClassTag

abstract class CSP extends Struct {
  def vars: Seq[Expr[Literal]] = varMap.values.toSeq
  val varMap: MMap[Any, Expr[Literal]] = MMap()
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
    varMap.get(id).orElse(parent.flatMap(_.getVar(id)))
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
    varMap.update(id, e)
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

  def addAnonymousTimepoint(): Expr[Int] = {
    newInputSubjectTo[Int](LocalIdent.anonymous(), Tag.ofInt)(tp => temporalOrigin <= tp) // note: an anonymous time point might stretch beyond the temporal horizon
  }

  case class Wrapper(lv: LocalVar) {
    override def toString: String = lv.id.name
  }

  def addVar(lv: LocalVar): Expr[Literal] = lv match {
    case lv @ LocalVar(id, tpe) if tpe.isSubtypeOf(ctx.predef.Time) =>
      assert(tpe == ctx.predef.Time)
      val e: Expr[Int] =
        if(lv == ctx.predef.Start)
          temporalOrigin
        else if(lv == ctx.predef.End)
          temporalHorizon
        else {
          newInputSubjectTo[Int](Wrapper(lv), Tag.ofInt)(tp =>
            temporalOrigin <= tp && tp <= temporalHorizon)
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
  implicit private def _predef = ctx.predef

  def encode(orig: core.Fluent): Expr[Fluent] =
    FluentF.ofExpr(orig.template, orig.params.map(ctx.encode(_)))

  def encode(orig: common.Constant): Expr[Fluent] =
    FluentF.ofExpr(orig.template, orig.params.map(ctx.encode(_)))

  def encode(orig: common.Interval[common.Expr]): IntervalF[Expr] =
    orig.map(ctx.encodeAsInt(_)) match {
      case ClosedInterval(s, e)    => IntervalF.ofExprUnwrapped(s, e)
      case LeftOpenInterval(s, e)  => IntervalF.ofExprUnwrapped(s - (-1), e)
      case RightOpenInterval(s, e) => IntervalF.ofExprUnwrapped(s, e - 1)
      case OpenInterval(s, e)      => IntervalF.ofExprUnwrapped(s - (-1), e - 1)
    }

  def encode(e: common.Expr): Expr[Literal] =
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
      case _: core.LocalVarDeclaration =>
      case _: core.ArgDeclaration      =>
//
      case core.TimedAssignmentAssertion(itv, fluent, value) =>
        val changeItv = encode(itv)
        val persistenceEnd = addAnonymousTimepoint() // anonymousTp().alwaysSubjectTo(changeItv.end <= _)
        addConstraint(changeItv.end <= persistenceEnd)
        val token =
          chronicles.EffTokF.ofExpr(changeItv.start,
                                    changeItv.end,
                                    persistenceEnd,
                                    encode(fluent),
                                    ctx.encode(value))

        addExport(token)
//
      case core.TimedEqualAssertion(itv, f, v) =>
        val interval = encode(itv)
        val token =
          chronicles.CondTokF.ofExpr(interval.start, interval.end, encode(f), ctx.encode(v))
        addExport(token)

      case core.TimedTransitionAssertion(ClosedInterval(s, e), f, v1, v2) =>
        val start = encodeAsInt(s)
        val cond =
          chronicles.CondTokF.ofExpr(start, start, encode(f), ctx.encode(v1))

        val changeItv = encode(LeftOpenInterval(s, e))
        val persistenceEnd = addAnonymousTimepoint() // anonymousTp().alwaysSubjectTo(changeItv.end <= _)
        addConstraint(changeItv.end <= persistenceEnd)
        val eff =
          chronicles.EffTokF.ofExpr(changeItv.start,
                                    changeItv.end,
                                    persistenceEnd,
                                    encode(f),
                                    ctx.encode(v2))

        addExport(cond)
        addExport(eff)
//
//      case core.StaticAssignmentAssertion(lhs, rhs) =>
//        val eff = SEffTokF.ofExpr(encode(lhs), ctx.encode(rhs))
//        copy(
//          staticEffects = eff :: staticEffects
//        )
      case core.StaticBooleanAssertion(e) =>
        val c = ctx.boolUnbox(encode(e))
        addConstraint(c)
    }
  def extendWithActions(template: core.ActionTemplate, howMany: Int)(
      implicit cnt: Counter): Unit = {
    for(i <- 0 until howMany) {
      val actId = cnt.next()
      val actName = s"${template.name}_$actId"

      val sub = addSub(actName)
      val act = ActionInstantiation.instance(template, actName)

      act.content.foreach {
        case core.LocalVarDeclaration(v) =>
          sub.addVar(v)
        case core.ArgDeclaration(a) =>
          val in = sub.newInput(a, ctx.specializedTags(a.typ))
          sub.recordVar(a, in)
        case _ =>
      }
      act.content.foreach {
        case _: core.LocalVarDeclaration => // already processed
        case _: core.ArgDeclaration      => // already processed

        // need action
        case statement: core.Statement => sub.extendWith(statement)
      }
      val operator: Expr[chronicles.Operator] = {
        dahu.model.input.Product(
          chronicles.OperatorF[Expr](
            Cst(template.name),
            dahu.model.input
              .Sequence[Literal](act.args.map(sub.getVar(_).get))(
                ctx.topTag,
                implicitly[ClassTag[Vec[Literal]]]),
            ctx.intUnbox(sub.getVar(act.start).get),
            ctx.intUnbox(sub.getVar(act.end).get)
          )
        )(chronicles.OperatorF.tag)
      }
      sub.addExport(operator)
//
//      val argsRewrite: Arg => Expr[Literal] = {
//        case a @ Arg(_, tpe) => ??? //Input(Ident(a))(ctx.specializedTags(tpe))
//      }
//
//      val chronicle = act.content.foldLeft(ChronicleFactory.empty(ctx)) {
//        case (c, s) => c.extended(s)(argsRewrite, cnt)
//      }
    }
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
