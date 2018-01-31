package dahu.constraints

import dahu.expr.labels.Labels.Value
import dahu.expr.types.TagIsoInt
import dahu.expr._
import dahu.recursion.ComputationF
import dahu.utils.Errors.unexpected

import dahu.constraints.domains._

trait Propagator {}

object Propagator {
  def forward(fun: Fun[_]): ForwardPropagator = fun match {
    case int.Add  => AddForwardPropagator
    case int.LEQ  => LEQForwardPropagator
    case int.EQ   => EqForwardPropagator
    case bool.Or  => OrForwardPropagator
    case bool.And => AndForwardPropagator
    case bool.Not => NotForwardPropagator
    case f =>
      println(s"warning: default forward propagator for $fun")
      ForwardPropagator.default(f).getOrElse(unexpected("No propagator for $f"))

  }
  def backward(fun: Fun[_]): BackwardPropagator = fun match {
    case int.Add  => AddBackwardPropagator
    case int.LEQ  => LEQBackwardPropagator
    case int.EQ   => EqBackwardPropagator
    case bool.Or  => OrBackwardPropagator
    case bool.And => AndBackwardPropagator
    case bool.Not => NotBackwardPropagator
    case _ =>
      println(s"warning: no backward propagator for $fun")
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
            override def outToInt(out: Value): Int               = ot.toInt(out)
          })
        case _ => None
      }
  }
}

trait ForwardPropagator {
  def propagate[T](args: Seq[T], dom: T => IntDomain): IntDomain
}
object ForwardPropagator {

  def default(f: Fun[_]): Option[ForwardPropagator] = {
    IntCompatibleFunc
      .compat(f)
      .map(translator =>
        new ForwardPropagator {
          override def propagate[T](args: Seq[T], dom: T => IntDomain): IntDomain =
            if(args.map(dom).forall(_.isSingleton)) {
              val argValues: Seq[Value] = translator.argsFromInt(args.map(a => dom(a).head))
              val eval                  = translator.outToInt(Value(f.compute(argValues)))
              SingletonDomain(eval)
            } else {
              FullDomain
            }
      })
  }

}
trait BackwardPropagator {
  def propagate[T](args: Seq[T], out: T, dom: T => IntDomain): Seq[IntDomain]
}
object BackwardPropagator {
  case object NoOp extends BackwardPropagator {
    override def propagate[T](args: Seq[T], otr: T, dom: T => IntDomain): Seq[IntDomain] =
      args.map(dom)
  }
}

abstract class ForwardPropagator1 extends ForwardPropagator {
  def propagate(d: IntDomain): IntDomain

  override def propagate[T](args: Seq[T], dom: T => IntDomain): IntDomain =
    propagate(dom(args.head))
}
abstract class ForwardPropagator2 extends ForwardPropagator {
  def propagate(l: IntDomain, r: IntDomain): IntDomain
  def propagate[T](args: Seq[T], dom: T => IntDomain): IntDomain =
    propagate(dom(args(0)), dom(args(1)))
}
abstract class ForwardPropagatorN extends ForwardPropagator {
  def propagate(domains: Seq[IntDomain]): IntDomain
  def propagate[T](args: Seq[T], dom: T => IntDomain): IntDomain =
    propagate(args.map(dom))
}

abstract class BackwardPropagator1 extends BackwardPropagator {
  def propagate(in: IntDomain, out: IntDomain): IntDomain

  override def propagate[T](args: Seq[T], out: T, dom: T => IntDomain): Seq[IntDomain] =
    Array(propagate(dom(args.head), dom(out)))
}
abstract class BackwardPropagator2 extends BackwardPropagator {
  def propagate(in1: IntDomain, r: IntDomain, out: IntDomain): (IntDomain, IntDomain)

  override def propagate[T](args: Seq[T], out: T, dom: T => IntDomain): Seq[IntDomain] = {
    val (d1, d2) = propagate(dom(args(0)), dom(args(1)), dom(out))
    Seq(d1, d2)
  }
}
abstract class BackwardPropagatorN extends BackwardPropagator {
  def propagate(in: Seq[IntDomain], out: IntDomain): Seq[IntDomain]

  override def propagate[T](args: Seq[T], out: T, dom: T => IntDomain): Seq[IntDomain] = {
    propagate(args.map(dom), dom(out))
  }
}

case object AddForwardPropagator extends ForwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain): IntDomain = l + r
}

case object AddBackwardPropagator extends BackwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain, out: IntDomain): (IntDomain, IntDomain) = {
    (
      l & (out - r),
      r & (out - l)
    )
  }
}

case object LEQForwardPropagator extends ForwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain): IntDomain =
    if(l.ub <= r.lb) True
    else if(l.lb > r.ub) False
    else TrueOrFalse
}
case object LEQBackwardPropagator extends BackwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain, out: IntDomain): (IntDomain, IntDomain) = {
    if(out == True)
      (IntervalDomain(l.lb, math.min(l.ub, r.ub)), IntervalDomain(math.max(r.lb, l.lb), r.ub))
    else if(out == False)
      (IntervalDomain(math.max(l.lb, r.lb + 1), l.ub),
       IntervalDomain(r.lb, math.min(r.ub, l.ub - 1)))
    else
      (l, r)
  }
}

case object OrForwardPropagator extends ForwardPropagatorN {
  override def propagate(domains: Seq[IntDomain]): IntDomain = domains.foldLeft(False)(_.max(_))
}

case object OrBackwardPropagator extends BackwardPropagatorN {
  override def propagate(domains: Seq[IntDomain], out: IntDomain): Seq[IntDomain] = {
    if(out == True) {
      if(domains.contains(True))
        domains
      else if(domains.count(_.size > 1) == 1)
        domains.map(x => if(x.size > 1) True else x)
      else if(domains.forall(_ == False))
        domains.map(_ => EmptyDomain)
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
  override def propagate(domains: Seq[IntDomain]): IntDomain = domains.foldLeft(True)(_.min(_))
}

case object AndBackwardPropagator extends BackwardPropagatorN {

  override def propagate(domains: Seq[IntDomain], out: IntDomain): Seq[IntDomain] = {
    if(out == True) {
      domains.map(x => True)
    } else if(out == False) {
      if(domains.contains(False))
        domains
      else if(domains.count(_.size > 1) == 1)
        domains.map(x => if(x.size > 1) False else x)
      else if(domains.forall(_ == True))
        domains.map(_ => EmptyDomain)
      else
        domains
    } else {
      domains
    }
  }
}

case object NotForwardPropagator extends ForwardPropagator1 {
  override def propagate(d: IntDomain): IntDomain =
    if(d == False) True
    else if(d == True) False
    else TrueOrFalse
}

case object NotBackwardPropagator extends BackwardPropagator1 {
  override def propagate(in: IntDomain, out: IntDomain): IntDomain =
    if(out == False) True
    else if(out == True) False
    else TrueOrFalse
}

case object EqForwardPropagator extends ForwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain): IntDomain =
    if(l.isSingleton && r.isSingleton && l.lb == r.lb)
      True
    else if(l.ub < r.lb || r.ub < l.lb)
      False
    else
      TrueOrFalse
}

case object EqBackwardPropagator extends BackwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain, out: IntDomain): (IntDomain, IntDomain) =
    if(out == True) {
      val d = l & r
      (d, d)
    } else if(out == False) {
      if(l.isSingleton && r.isSingleton && l == r) (EmptyDomain, EmptyDomain)
      else if(l.isSingleton) (l, r \ l.head)
      else if(r.isSingleton) (l \ r.head, r)
      else (l, r)
    } else {
      (l, r)
    }
}
