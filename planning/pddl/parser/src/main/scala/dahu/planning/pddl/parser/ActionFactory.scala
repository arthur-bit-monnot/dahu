package dahu.planning.pddl.parser

import dahu.planning.model.common._
import dahu.planning.model.full._
import Utils._
import fr.uga.pddl4j.parser.{Exp, Op}
import dahu.utils.errors._

import scala.collection.JavaConverters._

class ActionFactory(actionName: String, parent: Resolver, model: Model) extends Factory {
  implicit def predef = parent.predef
  private var template = ActionTemplate(model.scope / actionName, model)

  private val start = LocalVar(resolver.id(predef.StartSym), resolver.predef.Time)
  private val end = LocalVar(resolver.id(predef.EndSym), resolver.predef.Time)

  rec(LocalVarDeclaration(start))
  rec(LocalVarDeclaration(end))

  override def context: ActionTemplate = template

  override def getTranslator(name: String): FunctionCompat = parent.getTranslator(name)

  private def duration: StaticExpr = BinaryExprTree(operators.Sub, end, start)

  def rec(block: InActionBlock): Unit = {
    template = template + block
  }

  def preprocess(op: Op): IntermediateAction = {
    require(op.getName.getImage == actionName)

    op.getParameters.asScala.foreach {
      case ast.TypedSymbol(name, tpe) =>
        rec(ArgDeclaration(Arg(resolver.id(name), resolver.typeOf(tpe))))
    }

    Option(op.getDuration) match {
      case Some(ast.Eq(ast.Duration(_), ast.Cst(cst))) =>
        rec(
          BooleanAssertion(BinaryExprTree(operators.Eq, duration, cst))
        )
      case Some(ast.Eq(ast.Duration(_), e @ ast.Fluent(f, args))) =>
        rec(
          TemporallyQualifiedAssertion(
            Equals(ClosedInterval(start, start)),
            TimedEqualAssertion(resolver.getTranslator(f).fluent(f, args, resolver),
                                duration,
                                Some(context),
                                model.scope.makeNewId())
          )
        )
      case None =>
        rec(
          BooleanAssertion(BinaryExprTree(operators.Eq, duration, IntLiteral(0)))
        )
    }
    val ass =
      Option(op.getDuration) match {
        case Some(_) =>
          assertionsInDurative(op.getPreconditions, op.getEffects)
        case None =>
          assertionsInInstantaneous(op.getPreconditions, op.getEffects)
      }

    val temporal = ass.collect { case ta: TAss   => ta }
    val atemporal = ass.collect { case ta: NTAss => ta }
    atemporal.foreach { ba =>
      rec(ba.ass)
    }

    IntermediateAction(context, start, end, temporal)
  }

  def asEffectAss(e: Exp): TimedAssignmentAssertion = e match {
    case ast.AssertionOnFunction(funcName) =>
      resolver.getTranslator(funcName).effect(e, resolver)
    case _ => unexpected
  }
  def asCondAss(e: Exp): TimedEqualAssertion = e match {
    case ast.AssertionOnFunction(funcName) =>
      resolver.getTranslator(funcName).condition(e, resolver)
    case _ => unexpected
  }
  def assertionsInDurative(conds: Exp, effs: Exp): Seq[Ass] = {
    def getPre(pre: Exp): Seq[Ass] = pre match {
      case ast.And(subs) => subs.flatMap(getPre)
      case ast.AtStart(e @ ast.AssertionOnFunction(funcName)) =>
        Ass(TQual.Start, asCondAss(e)) :: Nil
      case ast.AtEnd(e @ ast.AssertionOnFunction(funcName)) =>
        Ass(TQual.End, asCondAss(e)) :: Nil
      case ast.OverAll(e @ ast.AssertionOnFunction(funcName)) =>
        Ass(TQual.All, asCondAss(e)) :: Nil
      case ast.OverAll(ast.BooleanExpr(ba)) =>
        NTAss(BooleanAssertion(ba)) :: Nil
      case _ => unsupported(pre.toString)
    }
    def getEff(pre: Exp): Seq[Ass] = pre match {
      case ast.And(subs) => subs.flatMap(getEff)
      case ast.AtStart(e @ ast.AssertionOnFunction(funcName)) =>
        Ass(TQual.Start, asEffectAss(e)) :: Nil
      case ast.AtEnd(e @ ast.AssertionOnFunction(funcName)) =>
        Ass(TQual.End, asEffectAss(e)) :: Nil
      case ast.OverAll(e @ ast.AssertionOnFunction(funcName)) =>
        Ass(TQual.All, asEffectAss(e)) :: Nil
    }
    getPre(conds) ++ getEff(effs)
  }
  def assertionsInInstantaneous(conds: Exp, effs: Exp): Seq[Ass] = {
    def getPre(pre: Exp): Seq[Ass] = pre match {
      case ast.And(subs) => subs.flatMap(getPre)
      case e             => Ass(TQual.Start, asCondAss(e)) :: Nil
    }
    def getEff(pre: Exp): Seq[Ass] = pre match {
      case ast.And(subs) => subs.flatMap(getEff)
      case e             => Ass(TQual.Start, asEffectAss(e)) :: Nil
    }
    getPre(conds) ++ getEff(effs)
  }
}

object ActionFactory {

  def build(op: Op, resolver: Resolver, model: Model): ActionTemplate = {
    implicit val predef: PddlPredef = resolver.predef
    val pre = preProcess(op, resolver, model)
    postProcess(pre)
  }

  def preProcess(op: Op, resolver: Resolver, model: Model): IntermediateAction = {
    val fact = new ActionFactory(op.getName.getImage, resolver, model)
    fact.preprocess(op)
  }

  def postProcess(act: IntermediateAction)(implicit predef: PddlPredef): ActionTemplate = {
    var mod = act.base
    val start = act.start
    val end = act.end
    def add(t: TemporalQualifier, e: TimedAssertion): Unit = {
      mod = mod + TemporallyQualifiedAssertion(t, e)
    }
    def regroup(qual: TQual, cond: TimedEqualAssertion, eff: TimedAssignmentAssertion): TAss = {
      assert(cond.fluent == eff.fluent)
      TAss(qual,
           TimedTransitionAssertion(cond.fluent,
                                    cond.right,
                                    eff.to,
                                    Some(act.base),
                                    act.base.scope.makeNewId()))
    }

    val merged: Iterable[Ass] = act.assertions
      .groupBy(a => (a.qual, a.ass.fluent))
      .values
      .map(_.toList)
      .map({
        case e :: Nil => e
        case TAss(t, cond: TimedEqualAssertion) :: TAss(t2, eff: TimedAssignmentAssertion) :: Nil =>
          assert(t == t2 && cond.fluent == eff.fluent)
          regroup(t, cond, eff)
        case TAss(t2, eff: TimedAssignmentAssertion) :: TAss(t, cond: TimedEqualAssertion) :: Nil =>
          assert(t == t2 && cond.fluent == eff.fluent)
          regroup(t, cond, eff)
        case _ => ???
      })

    val afterStart = BinaryExprTree(operators.Add, start, predef.Epsilon)
    val afterEnd = BinaryExprTree(operators.Add, end, predef.Epsilon)

    import TQual._
    for(ass <- merged) ass match {
      case TAss(Start, e: TimedEqualAssertion) =>
        add(Equals(ClosedInterval(start, afterStart)), e)
      case TAss(End, e: TimedEqualAssertion) =>
        add(Equals(ClosedInterval(end, afterEnd)), e)
      case TAss(All, e: TimedEqualAssertion) =>
        add(Equals(ClosedInterval(start, end)), e) //TODO: probably does not match pddl semantics
      case TAss(Start, e: TimedAssignmentAssertion) =>
        add(Equals(LeftOpenInterval(start, afterStart)), e)
      case TAss(End, e: TimedAssignmentAssertion) =>
        add(Equals(LeftOpenInterval(end, afterEnd)), e)
      case TAss(Start, e: TimedTransitionAssertion) =>
        add(Equals(ClosedInterval(start, afterStart)), e)
      case TAss(End, e: TimedTransitionAssertion) =>
        add(Equals(ClosedInterval(end, afterEnd)), e)
      case _ => ???
    }
    mod
  }

  type PASS = Seq[Ass] => Seq[Ass]
  case class Val(t: TQual, v: StaticExpr)
  case class Reqs(fluent: TimedExpr, cond: Seq[Val], effs: Seq[Val]) {
    override def toString: String = s"$fluent\n  ${cond.mkString("  ")}\n  ${effs.mkString("  ")}"
  }

}

sealed trait TQual
object TQual {
  case object Start extends TQual
  case object End extends TQual
  case object All extends TQual
}

/** Assertion */
sealed trait Ass
object Ass {
  def apply(qual: TQual, ass: TimedAssertion): TAss = TAss(qual, ass)
}

/** Non-temporal assertion */
case class NTAss(ass: StaticAssertion) extends Ass

/** Temporal assertion */
case class TAss(qual: TQual, ass: TimedAssertion) extends Ass

case class IntermediateAction(base: ActionTemplate,
                              start: LocalVar,
                              end: LocalVar,
                              assertions: Seq[TAss])
