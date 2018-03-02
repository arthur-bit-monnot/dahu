package dahu.constraints

import dahu.model.types._
import dahu.model.math._
import dahu.utils.errors.unexpected
import dahu.constraints.interval._
import BooleanDomain._
import dahu.model.functions._
import dahu.utils.debug._

trait Propagator {}

object Propagator {
  // TODO: make type safe and fix propagators of add/times monoids
  def forward(fun: Fun[_]): ForwardPropagator = fun match {
    case int.Add    => AddForwardPropagator
    case int.LEQ    => LEQForwardPropagator
    case int.EQ     => EqForwardPropagator
    case int.Negate => NegForwardPropagator
    case bool.Or    => OrForwardPropagator
    case bool.And   => AndForwardPropagator
    case bool.Not   => NotForwardPropagator
    case f =>
      warning(s"default forward propagator for $fun")
      ForwardPropagator.default(f).getOrElse(unexpected("No propagator for $f"))

  }
  def backward(fun: Fun[_]): BackwardPropagator = fun match {
    case int.Add    => AddBackwardPropagator
    case int.LEQ    => LEQBackwardPropagator
    case int.EQ     => EqBackwardPropagator
    case int.Negate => NegBackwardPropagator
    case bool.Or    => OrBackwardPropagator
    case bool.And   => AndBackwardPropagator
    case bool.Not   => NotBackwardPropagator
    case _ =>
      warning(s"no backward propagator for $fun")
      BackwardPropagator.NoOp
  }
}

abstract class IntCompatibilityLayer {
  def argsFromInt(args: Seq[Int]): Seq[Value]
  def outToInt(out: Value): Int
}
object IntCompatibleFunc {
  type IsoInt = TagIsoInt[Value]
  @unchecked
  def compat(function: Fun[_]): Option[IntCompatibilityLayer] = function match {
    case f: FunN[_, _] =>
      (f.inTypes, f.outType) match {
        case (it: IsoInt @unchecked, ot: IsoInt @unchecked) =>
          Some(new IntCompatibilityLayer {
            override def argsFromInt(args: Seq[Int]): Seq[Value] =
              args.map(a => Value(it.fromInt(a)))
            override def outToInt(out: Value): Int = ot.toInt(out)
          })
        case _ => None
      }
    case f: Fun2[_, _, _] =>
      (f.inType1, f.inType2, f.outType) match {
        case (it1: IsoInt @unchecked, it2: IsoInt @unchecked, ot: IsoInt @unchecked) =>
          Some(new IntCompatibilityLayer {
            override def argsFromInt(args: Seq[Int]): Seq[Value] = Seq(
              Value(it1.fromInt(args(0))),
              Value(it2.fromInt(args(1)))
            )
            override def outToInt(out: Value): Int = ot.toInt(out)
          })
        case _ => None
      }
    case f: Fun1[_, _] =>
      (f.inType, f.outType) match {
        case (it: IsoInt @unchecked, ot: IsoInt @unchecked) =>
          Some(new IntCompatibilityLayer {
            override def argsFromInt(args: Seq[Int]): Seq[Value] = Seq(Value(it.fromInt(args.head)))
            override def outToInt(out: Value): Int = ot.toInt(out)
          })
        case _ => None
      }
  }

}

abstract class IntsToIntFunction {
  def apply(params: Array[Int]): Int
}
object Types {
  type Var = Int
  type Val = Int
  type Evaluator = Array[Val] => Val // FunN[Int,Int]
  type EvaluatorX = (Array[Val], Val => Var) => Val
  type FwProp = (Array[Var], Var => Interval) => Interval
  type BwProp = (Array[Var], Var, Var => Interval) => Array[Interval]
}

trait ForwardPropagator {
  def propagate[T](args: Seq[T], dom: T => Interval): Interval
}
object ForwardPropagator {

  def default(f: Fun[_]): Option[ForwardPropagator] = {
    IntCompatibleFunc
      .compat(f)
      .map(translator =>
        new ForwardPropagator {
          override def propagate[T](args: Seq[T], dom: T => Interval): Interval =
            if(args.map(dom).forall(_.isSingleton)) {
              val argValues: Seq[Value] = translator.argsFromInt(args.map(a => dom(a).lb))
              val eval = translator.outToInt(Value(f.compute(argValues)))
              Interval(eval)
            } else {
              Interval.full
            }
      })
  }

}
trait BackwardPropagator {
  def propagate[T](args: Array[T], out: T, dom: T => Interval): Array[Interval]
}
object BackwardPropagator {
  case object NoOp extends BackwardPropagator {
    override def propagate[T](args: Array[T], otr: T, dom: T => Interval): Array[Interval] =
      args.map(dom)
  }
}

abstract class ForwardPropagator1 extends ForwardPropagator {
  def propagate(d: Interval): Interval

  override def propagate[T](args: Seq[T], dom: T => Interval): Interval =
    propagate(dom(args.head))
}
abstract class ForwardPropagator2 extends ForwardPropagator {
  def propagate(l: Interval, r: Interval): Interval
  def propagate[T](args: Seq[T], dom: T => Interval): Interval =
    propagate(dom(args(0)), dom(args(1)))
}
abstract class ForwardPropagatorN extends ForwardPropagator {
  def propagate(domains: Seq[Interval]): Interval
  def propagate[T](args: Seq[T], dom: T => Interval): Interval =
    propagate(args.map(dom))
}

abstract class BackwardPropagator1 extends BackwardPropagator {
  def propagate(in: Interval, out: Interval): Interval

  override def propagate[T](args: Array[T], out: T, dom: T => Interval): Array[Interval] =
    Array(propagate(dom(args.head), dom(out)))
}
abstract class BackwardPropagator2 extends BackwardPropagator {
  def propagate(in1: Interval, r: Interval, out: Interval): (Interval, Interval)

  override def propagate[T](args: Array[T], out: T, dom: T => Interval): Array[Interval] = {
    val (d1, d2) = propagate(dom(args(0)), dom(args(1)), dom(out))
    Array(d1, d2)
  }
}
abstract class BackwardPropagatorN extends BackwardPropagator {
  def propagate(in: Array[Interval], out: Interval): Array[Interval]

  override def propagate[T](args: Array[T], out: T, dom: T => Interval): Array[Interval] = {
    propagate(args.map(dom), dom(out))
  }
}

case object AddForwardPropagator extends ForwardPropagator2 {
  override def propagate(l: Interval, r: Interval): Interval = l plus r
}

case object AddBackwardPropagator extends BackwardPropagator2 {
  override def propagate(l: Interval, r: Interval, out: Interval): (Interval, Interval) = {
    (
      l inter (out minus r),
      r inter (out minus l)
    )
  }
}

case object LEQForwardPropagator extends ForwardPropagator2 {
  override def propagate(l: Interval, r: Interval): BooleanDomain =
    if(l.ub <= r.lb) BooleanDomain.True
    else if(l.lb > r.ub) BooleanDomain.False
    else BooleanDomain.Unknown
}
case object LEQBackwardPropagator extends BackwardPropagator2 {
  override def propagate(l: Interval, r: Interval, out: Interval): (Interval, Interval) = {
    if(out == True)
      (Interval(l.lb, math.min(l.ub, r.ub)), Interval(math.max(r.lb, l.lb), r.ub))
    else if(out == False)
      (Interval(math.max(l.lb, r.lb + 1), l.ub), Interval(r.lb, math.min(r.ub, l.ub - 1)))
    else
      (l, r)
  }
}

case object OrForwardPropagator extends ForwardPropagatorN {
  override def propagate(domains: Seq[Interval]): Interval =
    BooleanDomain.asBooleanDomains(domains.toArray).foldLeft(False)(_.or(_))
}

case object OrBackwardPropagator extends BackwardPropagatorN {
  override def propagate(domains: Array[Interval], out: Interval): Array[Interval] = {
    if(out == True) {
      if(domains.contains(True))
        domains
      else if(domains.count(_.size > 1) == 1)
        domains.map(x => if(x.size > 1) True else x)
      else if(domains.forall(_ == False))
        domains.map(_ => BooleanDomain.empty)
      else
        domains
    } else if(out == False) {
      domains.map(_ => False)
    } else {
      domains
    }
  }
}

case object AndForwardPropagator extends ForwardPropagatorN {
  override def propagate(domains: Seq[Interval]): Interval =
    BooleanDomain.asBooleanDomains(domains.toArray).foldLeft(True)(_.and(_))
}

case object AndBackwardPropagator extends BackwardPropagatorN {

  override def propagate(domains: Array[Interval], out: Interval): Array[Interval] = {
    if(out == True) {
      domains.map(x => True)
    } else if(out == False) {
      if(domains.contains(False))
        domains
      else if(domains.count(_.size > 1) == 1)
        domains.map(x => if(x.size > 1) False else x)
      else if(domains.forall(_ == True))
        domains.map(_ => BooleanDomain.empty)
      else
        domains
    } else {
      domains
    }
  }
}

case object NotForwardPropagator extends ForwardPropagator1 {
  override def propagate(d: Interval): BooleanDomain =
    if(d == False) True
    else if(d == True) False
    else BooleanDomain.Unknown
}

case object NotBackwardPropagator extends BackwardPropagator1 {
  override def propagate(in: Interval, out: Interval): BooleanDomain =
    if(out == False) True
    else if(out == True) False
    else BooleanDomain.Unknown
}

case object NegForwardPropagator extends ForwardPropagator1 {
  override def propagate(d: Interval): Interval = d.negated
}
case object NegBackwardPropagator extends BackwardPropagator1 {
  override def propagate(in: Interval, out: Interval): Interval = out.negated.inter(in)
}

case object EqForwardPropagator extends ForwardPropagator2 {
  override def propagate(l: Interval, r: Interval): BooleanDomain =
    if(l.isSingleton && r.isSingleton && l.lb == r.lb)
      True
    else if(l.ub < r.lb || r.ub < l.lb)
      False
    else
      Unknown
}

case object EqBackwardPropagator extends BackwardPropagator2 {
  override def propagate(l: Interval, r: Interval, out: Interval): (Interval, Interval) =
    if(out == True) {
      val d = l inter r
      (d, d)
    } else if(out == False) {
      if(l.isSingleton && r.isSingleton && l == r) (empty, empty)
      else if(l.isSingleton) (l, r withoutApproximation l)
      else if(r.isSingleton) (l withoutApproximation r, r)
      else (l, r)
    } else {
      (l, r)
    }
}
