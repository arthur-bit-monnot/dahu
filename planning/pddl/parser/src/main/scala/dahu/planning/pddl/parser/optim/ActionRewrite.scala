package dahu.planning.pddl.parser.optim

import dahu.planning.model.common.Interval.{ClosedOnLeft, ClosedOnRight, OpenOnLeft, OpenOnRight}
import dahu.planning.model.common._
import dahu.planning.model.core._
import dahu.planning.pddl.parser.{Options, PddlPredef}
import spire.syntax.cfor

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

class ActionRewrite(options: Options) {

  def optimize(model: CoreModel)(implicit predef: Predef): Try[CoreModel] = Try {
    val invariants = new InvariantInference(model)
//    implicit val ctx: Ctx = model
    val f: InModuleBlock => InModuleBlock = {
      case e: ActionTemplate => opt(e, invariants)
      case x                 => x
    }
    model.map(f)
  }

  def opt(a: ActionTemplate, invariants: InvariantInference)(
      implicit predef: Predef): ActionTemplate = {

    def println(str: Any): Unit = Predef.println(str.toString.replaceAll(a.name + "\\.", ""))
    val timelines = a.content
      .collect { case x: TimedAssertion => x }
      .map(assertionToTimeline)
      .sortBy(_.toString)

    val regrouped = regroup(timelines).sortBy(_.toString)
    val merged = removeDiscontinuitiesUnsafe(regrouped, a.start, a.end)
    val content = a.content.filterNot(_.isInstanceOf[TimedAssertion]) ++ merged.flatMap(encode)
    ActionTemplate(a.scope, content)
  }

  def removeDiscontinuitiesUnsafe(timelines: Seq[Timeline],
                                  start: Expr,
                                  end: Expr): Seq[Timeline] = {
    val ordering: (Expr, Expr) => PartialOrdering = {
      case (l, r) if l == start && r == end => After
      case (l, r) if r == start && l == end => Before
      case _                                => Unordered
    }
    def merge(before: Timeline, after: Timeline): Timeline = {
      assert(before.fluent == after.fluent)
      val header = before.toks.dropRight(1)
      val footer = after.toks.drop(1)
      val l = before.toks.last
      val r = after.toks.head
      val middleToks = (l, r) match {
        case (Is(itv: OpenOnRight, v), Undef(itv2: OpenOnRight)) =>
          Is(itv, v) :: Undef(RightOpenInterval(itv.end, itv2.end)) :: Nil
        case (Is(itv: ClosedOnRight, v), Undef(itv2: OpenOnRight)) =>
          Is(itv, v) :: Undef(OpenInterval(itv.end, itv2.end)) :: Nil
        case (lhs @ Becomes(t, v), rhs @ Undef(itv: OpenOnLeft)) =>
          lhs :: Is(LeftOpenInterval(t, itv.start), v) :: rhs :: Nil
        case (lhs @ Becomes(t, v), rhs @ Undef(itv: ClosedOnLeft)) =>
          lhs :: Is(OpenInterval(t, itv.start), v) :: rhs :: Nil
      }
      Timeline(before.fluent, header ++ middleToks ++ footer: _*)
    }

    timelines.groupBy(_.fluent).values.toSeq.map {
      case Seq(tl)       => tl
      case Seq(tl1, tl2) =>
//        println(s"warning: merging two discontinuous timelines: $tl1 ----- $tl2")
        tl1.order(tl2, ordering) match {
          case After     => merge(tl1, tl2)
          case Before    => merge(tl2, tl1)
          case Unordered => ???
        }
    }
  }

  def encode(tl: Timeline)(implicit predef: Predef): Seq[TimedAssertion] = {
    val fluent = tl.fluent
//    def id(): Id = ctx.scope.makeNewId()
    def go(toks: List[Token]): List[TimedAssertion] = toks match {

      case Is(Point(a), from) :: Undef(_) :: Becomes(b, to) :: rest =>
        TimedTransitionAssertion(ClosedInterval(a, b), fluent, from, to) :: go(rest)

      case Undef(itv) :: Becomes(a, v) :: rest =>
        if(itv.isLeftOpen)
          TimedAssignmentAssertion(LeftOpenInterval(itv.start, a), tl.fluent, v) :: go(rest)
        else
          TimedAssignmentAssertion(ClosedInterval(itv.start, a), tl.fluent, v) :: go(rest)
      case Becomes(a, v) :: rest =>
        TimedAssignmentAssertion(Point(a), tl.fluent, v) :: go(rest)
      case Is(itv, v) :: rest =>
        TimedEqualAssertion(itv, tl.fluent, v) :: go(rest)
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

  def assertionToTimeline(e: TimedAssertion): Timeline = e match {
    case TimedEqualAssertion(itv, f, v) =>
      Timeline(f, Is(itv, v))

    case TimedAssignmentAssertion(itv, f, v) =>
      itv match {
        case Point(t) => Timeline(f, Becomes(t, v))
        case ClosedInterval(s, e) =>
          Timeline(f, Undef(RightOpenInterval(s, e)), Becomes(e, v))
        case LeftOpenInterval(s, e) =>
          Timeline(f, Undef(OpenInterval(s, e)), Becomes(e, v))
        case _ => ???
      }

    case TimedTransitionAssertion(itv, f, from, to) =>
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
  def itv: Interval[Expr]
  def rightBefore(next: Token): Boolean =
    itv.end == next.itv.start && itv.isRightOpen != next.itv.isLeftOpen

}
case class Undef(itv: Interval[Expr] with OpenOnRight) extends Token with AfterIs
case class Becomes(at: Expr, v: Expr) extends Token with AfterIs {
  def itv: Interval[Expr] = Point(at)
}
case class Is(itv: Interval[Expr], v: Expr) extends Token

class Timeline(val fluent: Fluent, val toks: Seq[Token]) {
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

  def order(tl: Timeline, tpOrder: (Expr, Expr) => PartialOrdering): PartialOrdering = {
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
  def apply(fluent: Fluent, toks: Token*): Timeline = new Timeline(fluent, toks.toVector)
  def unapply(tl: Timeline): Option[(Fluent, Seq[Token])] = Some((tl.fluent, tl.toks))
}

sealed trait PartialOrdering
case object RightAfter extends PartialOrdering
case object RightBefore extends PartialOrdering
case object Unordered extends PartialOrdering
case object After extends PartialOrdering
case object Before extends PartialOrdering
