package dahu.planning.pddl.parser.optim

import dahu.planning.model.common.Interval.OpenOnRight
import dahu.planning.model.common._
import dahu.planning.model.full._
import dahu.planning.pddl.parser.{Options, PddlPredef}
import spire.syntax.cfor

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

class ActionRewrite(options: Options) {
  def optimize(model: Model)(implicit predef: Predef): Try[Model] = Try {
    implicit val ctx: Ctx = model
    val f: InModuleBlock => InModuleBlock = {
      case e: ActionTemplate => opt(e)
      case x                 => x
    }
    model.map(f)
  }

  def opt(a: ActionTemplate)(implicit predef: Predef, ctx: Ctx): ActionTemplate = {
    def println(str: Any): Unit = Predef.println(str.toString.replaceAll(a.name + "\\.", ""))
    val timelines = a.store.blocks
      .collect { case x: TemporallyQualifiedAssertion => x }
      .map(assertionToTimeline)
      .sortBy(_.toString)

    if(options.lint) {
      val regrouped = regroup(timelines).sortBy(_.toString)
//      timelines.foreach(println)
//      println("  ============  ")
      regrouped.foreach(println)
      println("")
      regrouped.map(encode).foreach(x => println(x.mkString("     ------     ")))
      println("")
    }
//    println(xx)
    a
  }

  def removeDiscontinuitiesUnsafe(timelines: Seq[Timeline],
                                  start: StaticExpr,
                                  end: StaticExpr): Seq[Timeline] = {
    val ordering: (StaticExpr, StaticExpr) => PartialOrdering = {
      case (l, r) if l == start && r == end => After
      case (l, r) if r == start && l == end => Before
      case _                                => Unordered
    }
    timelines.groupBy(_.fluent).values.toSeq.map {
      case Seq(tl) => tl
      case Seq(tl1, tl2) =>
        tl1.order(tl2, ordering) match {
          case After     => ???
          case Before    => ???
          case Unordered => ???
        }
    }
  }

  private def tqa(itv: Interval[StaticExpr],
                  assertion: TimedAssertion): TemporallyQualifiedAssertion =
    TemporallyQualifiedAssertion(Equals(itv), assertion)

  def encode(tl: Timeline)(implicit predef: Predef, ctx: Ctx): Seq[TemporallyQualifiedAssertion] = {
    val fluent = tl.fluent
    def id(): Id = ctx.scope.makeNewId()
    def go(toks: List[Token]): List[TemporallyQualifiedAssertion] = toks match {

      case Is(Point(a), from) :: Undef(_) :: Becomes(b, to) :: rest =>
        tqa(
          ClosedInterval(a, b),
          TimedTransitionAssertion(fluent, from, to, Some(ctx), id())
        ) :: go(rest)

      case Undef(itv) :: Becomes(a, v) :: rest =>
        if(itv.isLeftOpen)
          tqa(LeftOpenInterval(itv.start, a),
              TimedAssignmentAssertion(tl.fluent, v, Some(ctx), id())) :: go(rest)
        else
          tqa(ClosedInterval(itv.start, a), TimedAssignmentAssertion(tl.fluent, v, Some(ctx), id())) :: go(
            rest)
      case Becomes(a, v) :: rest =>
        tqa(Point(a), TimedAssignmentAssertion(tl.fluent, v, Some(ctx), id())) :: go(rest)
      case Is(itv, v) :: rest =>
        tqa(itv, TimedEqualAssertion(tl.fluent, v, Some(ctx), id())) :: go(rest)
      case Nil => Nil
    }
    go(tl.toks.toList)
  }

  @tailrec private def regroup(timelines: Seq[Timeline]): Seq[Timeline] = {
    val processed = mutable.Set[Timeline]()
    val res = mutable.ArrayBuffer[Timeline]()
    for(tl <- timelines; cand <- timelines) {
      if(tl == cand || processed(tl) || processed(cand)) {} else {
        tl.tryMerge(cand) match {
          case Some(combined) =>
            res += combined
            processed += tl
            processed += cand
          case None =>
        }
      }
    }
    if(processed.isEmpty) {
      assert(res.isEmpty)
      timelines
    } else {
      regroup(res ++ timelines.filterNot(processed))
    }
  }

  def assertionToTimeline(e: TemporallyQualifiedAssertion): Timeline = e match {
    case TemporallyQualifiedAssertion(Equals(itv), TimedEqualAssertion(f, v, _, _)) =>
      Timeline(f, Is(itv, v))

    case TemporallyQualifiedAssertion(Equals(itv), TimedAssignmentAssertion(f, v, _, _)) =>
      itv match {
        case Point(t) => Timeline(f, Becomes(t, v))
        case ClosedInterval(s, e) =>
          Timeline(f, Undef(RightOpenInterval(s, e)), Becomes(e, v))
        case LeftOpenInterval(s, e) =>
          Timeline(f, Undef(OpenInterval(s, e)), Becomes(e, v))
        case _ => ???
      }

    case TemporallyQualifiedAssertion(Equals(itv), TimedTransitionAssertion(f, from, to, _, _)) =>
      itv match {
        case Point(_) => ???
        case ClosedInterval(s, e) =>
          Timeline(
            f,
            Is(Point(s), from),
            Undef(OpenInterval(s, e)),
            Becomes(e, to)
          )
        case _ => ???
      }

  }
}

object Point {
  def apply[A](a: A): Interval[A] = ClosedInterval(a, a)
  def unapply[A](arg: Interval[A]): Option[A] = arg match {
    case ClosedInterval(a, b) if a == b => Some(a)
    case _                              => None
  }
}

sealed trait AfterIs
sealed trait Token {
  def itv: Interval[StaticExpr]
  def rightBefore(next: Token): Boolean =
    itv.end == next.itv.start && itv.isRightOpen != next.itv.isLeftOpen

}
case class Undef(itv: Interval[StaticExpr] with OpenOnRight) extends Token with AfterIs
case class Becomes(at: StaticExpr, v: StaticExpr) extends Token with AfterIs {
  def itv: Interval[StaticExpr] = Point(at)
}
case class Is(itv: Interval[StaticExpr], v: StaticExpr) extends Token

class Timeline(val fluent: TimedExpr, val toks: Seq[Token]) {
  // timeline must be continuous
  require(toks.nonEmpty)
  require((1 until toks.size).forall(i => toks(i - 1).rightBefore(toks(i))))

  override def toString: String = s"$fluent:    ${toks.mkString("  --  ")}"

  def tryMerge(other: Timeline): Option[Timeline] = other match {
    case Timeline(`fluent`, oToks) if toks.last.rightBefore(oToks.head) =>
      Some(Timeline(fluent, toks ++ oToks: _*))
    case Timeline(`fluent`, oToks) if oToks.last.rightBefore(toks.head) =>
      Some(Timeline(fluent, oToks ++ toks: _*))
    case _ => None

  }

  def order(tl: Timeline, tpOrder: (StaticExpr, StaticExpr) => PartialOrdering): PartialOrdering = {
    if(fluent != tl.fluent) Unordered
    else if(toks.last.rightBefore(tl.toks.head)) RightAfter
    else if(tl.toks.last.rightBefore(toks.head)) RightBefore
    else {
      val leftTps = this.toks.flatMap(t => Seq(t.itv.start, t.itv.end))
      val rightTps = tl.toks.flatMap(t => Seq(t.itv.start, t.itv.end))
      if(leftTps.exists(l => rightTps.exists(r => tpOrder(l, r) == After)))
        After
      else if(leftTps.exists(l => rightTps.exists(r => tpOrder(l, r) == Before)))
        Before
      else
        Unordered
    }
  }
}
object Timeline {
  def apply(fluent: TimedExpr, toks: Token*): Timeline = new Timeline(fluent, toks.toVector)
  def unapply(tl: Timeline): Option[(TimedExpr, Seq[Token])] = Some((tl.fluent, tl.toks))
}

sealed trait PartialOrdering
case object RightAfter extends PartialOrdering
case object RightBefore extends PartialOrdering
case object Unordered extends PartialOrdering
case object After extends PartialOrdering
case object Before extends PartialOrdering
