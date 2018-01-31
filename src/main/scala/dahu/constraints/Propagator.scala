package dahu.constraints

import dahu.expr.labels.Labels.Value
import dahu.expr.types.TagIsoInt
import dahu.expr._
import dahu.recursion.ComputationF
import dahu.utils.Errors.unexpected

case class IntDomain(min: Int, max: Int) {
  def this() = this(0, 100000)
  def this(v: Int) = this(v, v)

  def max(o: IntDomain): IntDomain = IntDomain(math.max(min, o.min), math.max(max, o.max))
  def min(o: IntDomain): IntDomain = IntDomain(math.min(min, o.min), math.min(max, o.max))
  def +(o: IntDomain)              = IntDomain(min + o.min, max + o.max)
  def negated: IntDomain           = IntDomain(-max, -min)
  def -(o: IntDomain): IntDomain   = this + o.negated
  def -(value: Int): IntDomain =
    if(value == min) IntDomain(min+1, max)
    else if(value == max) IntDomain(min, max-1)
    else this

  def inter(o: IntDomain): IntDomain = IntDomain(math.max(min, o.min), math.min(max, o.max))

  def isEmpty: Boolean = min > max
  def isSet: Boolean   = min == max

  override def equals(o: scala.Any): Boolean = o match {
    case IntDomain(omin, omax) =>
      (min > max && omin > omax) || (min == omin && max == omax)
  }

  def size: Int = math.max(0, max - min + 1)

  override def toString: String =
    if(isEmpty) "{}"
    else if(isSet) min.toString
    else s"[$min, $max]"
}
object BoolDomains {
  val True    = new IntDomain(1)
  val False   = new IntDomain(0)
  val Unknown = IntDomain(0, 1)

}

trait Propagator {}

object Propagator {
  def forward(fun: Fun[_]): ForwardPropagator = fun match {
    case int.Add  => AddForwardPropagator
    case int.LEQ  => LEQForwardPropagator
    case int.EQ => EqForwardPropagator
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
    case int.EQ => EqBackwardPropagator
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
            if(args.map(dom).forall(d => d.min == d.max)) {
              val argValues: Seq[Value] = translator.argsFromInt(args.map(a => dom(a).min))
              val eval                  = translator.outToInt(Value(f.compute(argValues)))
              new IntDomain(eval)
            } else {
              new IntDomain()
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
      l.inter(out - r),
      r.inter(out - l)
    )
  }
}

case object LEQForwardPropagator extends ForwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain): IntDomain =
    if(l.max <= r.min) BoolDomains.True
    else if(l.min > r.max) BoolDomains.False
    else BoolDomains.Unknown
}
case object LEQBackwardPropagator extends BackwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain, out: IntDomain): (IntDomain, IntDomain) =
    if(out == BoolDomains.True)
      (IntDomain(l.min, math.min(l.max, r.min)), IntDomain(math.max(r.min, l.max), r.max))
    else if(out == BoolDomains.False)
      (IntDomain(math.max(l.min, r.min + 1), l.max), IntDomain(r.min, math.min(r.max, l.max - 1)))
    else
      (l, r)
}

case object OrForwardPropagator extends ForwardPropagatorN {
  override def propagate(domains: Seq[IntDomain]): IntDomain = domains.tail.foldLeft(domains.head) {
    case (acc, d) => acc.max(d)
  }
}

//case object OrBackwardPropagator extends BackwardPropagator2 {
//  override def propagate(l: IntDomain, r: IntDomain, out: IntDomain): (IntDomain, IntDomain) =
//    (l.inter(out - r), r.inter(out - l))
//}

case object OrBackwardPropagator extends BackwardPropagatorN {
  import BoolDomains._
  override def propagate(domains: Seq[IntDomain], out: IntDomain): Seq[IntDomain] = {
    if(out == True) {
      if(domains.contains(True))
        domains
      else if(domains.count(_ == Unknown) == 1)
        domains.map(x => if(x == Unknown) True else x)
      else if(domains.forall(_ == False))
        domains.map(_ => IntDomain(1, 0))
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
  override def propagate(domains: Seq[IntDomain]): IntDomain = domains.tail.foldLeft(domains.head) {
    case (acc, d) => acc.min(d)
  }
}

case object AndBackwardPropagator extends BackwardPropagatorN {
  import BoolDomains._
  override def propagate(domains: Seq[IntDomain], out: IntDomain): Seq[IntDomain] = {
    if(out == BoolDomains.True) {
      domains.map(x => BoolDomains.True)
    } else if(out == BoolDomains.False) {
      if(domains.contains(False))
        domains
      else if(domains.count(_ == Unknown) == 1)
        domains.map(x => if(x == Unknown) False else x)
      else if(domains.forall(_ == True))
        domains.map(_ => IntDomain(1, 0))
      else
        domains
    } else {
      domains
    }
  }
}

case object NotForwardPropagator extends ForwardPropagator1 {
  override def propagate(d: IntDomain): IntDomain =
    if(d == BoolDomains.False) BoolDomains.True
    else if(d == BoolDomains.True) BoolDomains.False
    else BoolDomains.Unknown
}

case object NotBackwardPropagator extends BackwardPropagator1 {
  override def propagate(in: IntDomain, out: IntDomain): IntDomain =
    if(out == BoolDomains.False) BoolDomains.True
    else if(out == BoolDomains.True) BoolDomains.False
    else BoolDomains.Unknown
}

case object EqForwardPropagator extends ForwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain): IntDomain =
    if(l.isSet && r.isSet && l.min == r.min) BoolDomains.True
    else if(l.max < r.min || r.max < l.min) BoolDomains.False
    else BoolDomains.Unknown
}

case object EqBackwardPropagator extends BackwardPropagator2 {
  override def propagate(l: IntDomain, r: IntDomain, out: IntDomain): (IntDomain, IntDomain) =
    if(out == BoolDomains.True) {
      val d = l inter r
      (d, d)
    } else if(out == BoolDomains.False) {
      if(l.isSet) (l, r - l.min)
      else if(r.isSet) (l - r.min, r)
      else (l, r)
    } else {
      (l, r)
    }

}
