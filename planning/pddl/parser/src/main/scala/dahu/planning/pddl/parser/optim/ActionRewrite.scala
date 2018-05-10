package dahu.planning.pddl.parser.optim

import dahu.planning.model.common._
import dahu.planning.model.full._
import dahu.planning.pddl.parser.PddlPredef
import spire.syntax.cfor

import scala.annotation.tailrec
import scala.collection.mutable

trait ModelOptimizer {
  def optimize(model: Model): Model
}

object ActionRewrite extends ModelOptimizer {
  override def optimize(model: Model): Model = {
    val f: InModuleBlock => InModuleBlock = {
      case e: ActionTemplate => opt(e)
      case x                 => x
    }
    model.map(f)
  }

  def opt(a: ActionTemplate): ActionTemplate = {
    def println(str: Any): Unit = Predef.println(str.toString.replaceAll(a.name + ".", ""))
    println(a)
    val timelines = a.store.blocks
      .collect { case x: TemporallyQualifiedAssertion => x }
      .map(assertionToTimeline)
      .sortBy(_.toString)

    val regrouped = regroup(timelines).sortBy(_.toString)
    timelines.foreach(println)
    println("  ============  ")
    regrouped.foreach(println)
    println("")
//    println(xx)
    a
  }
  private def tqa(itv: Interval[StaticExpr], assertion: TimedAssertion): TemporallyQualifiedAssertion =
    TemporallyQualifiedAssertion(Equals(itv), assertion)

  def encode(tl: Timeline)(implicit predef: Predef): Seq[TemporallyQualifiedAssertion] = {
    val fluent = tl.fluent
    def go(toks: List[TToken]): List[TemporallyQualifiedAssertion] = toks match {
      case TToken(itv, Is(v)) :: rest =>
        tqa(itv, TimedEqualAssertion(tl.fluent, v, null, null)) :: go(rest)
    }
    go(tl.toks.toList)
  }

  @tailrec def regroup(timelines: Seq[Timeline]): Seq[Timeline] = {
    val processed = mutable.Set[Timeline]()
    val res = mutable.ArrayBuffer[Timeline]()
    for(tl <- timelines; cand <- timelines) {
      if(tl == cand || processed(tl) || processed(cand)) {

      } else {
        tl.tryMerge(cand) match {
          case Some(combined) => res += combined
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
      Timeline(f, TToken(itv, Is(v)))

    case TemporallyQualifiedAssertion(Equals(itv), TimedAssignmentAssertion(f, v, _, _)) =>
      itv match {
        case Point(t) => Timeline(f, TToken(Point(t), Becomes(v)))
        case ClosedInterval(s, e) => Timeline(f, TToken(RightOpenInterval(s,e), Undef), TToken(Point(e), Becomes(v)))
        case LeftOpenInterval(s, e) => Timeline(f, TToken(OpenInterval(s,e), Undef), TToken(Point(e), Becomes(v)))
        case _ => ???
      }

    case TemporallyQualifiedAssertion(Equals(itv), TimedTransitionAssertion(f, from, to, _, _)) =>
      itv match {
        case Point(_) => ???
        case ClosedInterval(s, e) => Timeline(
          f,
          TToken(Point(s), Is(from)),
          TToken(OpenInterval(s, e), Undef),
          TToken(Point(e), Becomes(to))
        )
        case _ => ???
      }

  }
}

object Point {
  def apply[A](a: A): Interval[A] = ClosedInterval(a,a)
  def unapply[A](arg: Interval[A]): Option[A] = arg match {
    case ClosedInterval(a, b) if a == b => Some(a)
    case _ => None
  }
}

sealed trait Token
case object Undef extends Token
case class Becomes(v: StaticExpr) extends Token
case class Is(v: StaticExpr) extends Token

case class TToken(itv: Interval[StaticExpr], tok: Token) {
  def rightBefore(next: TToken): Boolean = itv.end == next.itv.start && itv.isRightOpen != next.itv.isLeftOpen

  override def toString: String = s"$itv $tok"
}

class Timeline(val fluent: TimedExpr, val toks: Seq[TToken]) {
  // timeline must be continuous
  require(toks.nonEmpty)
  require((1 until toks.size).forall(i => toks(i-1).rightBefore(toks(i))))

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
  def apply(fluent: TimedExpr, toks: TToken*): Timeline = new Timeline(fluent, toks.toVector)
  def unapply(tl: Timeline): Option[(TimedExpr, Seq[TToken])] = Some((tl.fluent, tl.toks))
}

sealed trait PartialOrdering
case object RightAfter extends PartialOrdering
case object RightBefore extends PartialOrdering
case object Unordered extends PartialOrdering
case object After extends PartialOrdering
case object Before extends PartialOrdering

