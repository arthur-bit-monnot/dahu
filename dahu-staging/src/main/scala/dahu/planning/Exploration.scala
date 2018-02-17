package dahu.planning

import dahu.expr.{Expr, Input}

object Exploration {

  trait Container {
//    def variables: Seq[Expr[Any]]
    def presence: Expr[Boolean]
    def constraints: Expr[Boolean]
  }

  case class Interval(start: Timepoint, duration: Duration, end: Timepoint, presence: Presence)
      extends Container {
    import dahu.expr.dsl._

    val constraints: Expr[Boolean] = ((start + duration) === end) && (duration >= 0)
  }

  type Timepoint = Input[Int]
  type Duration  = Input[Int]
  type Presence  = Input[Boolean]

//  sealed trait Interval {
//    def start: Timepoint
//    def duration: Timepoint
//    def end: Timepoint
//    def present: Presence
//  }
//
//  sealed trait Token[Cat, Value] extends Interval {
//    def category: Cat
//    def value: Value
//  }
//
//  trait EffectToken[C, V] extends Token[C, V]
//  case class EffectTokenImpl[C, V](start: Timepoint ,
//                                   duration: Duration,
//                                   end: Timepoint,
//                                   present: Presence,
//                                   category: C,
//                                   value: V)
//      extends EffectToken[C, V]
//  trait ConditionToken[C, V] extends Token[C, V]
//
//  type Parameters = Array[Input[Any]]
//  type Assignment = Array[(Input[Any], Any)]
//
//  trait Parameterized[Y] {
//    def parameters: Parameters
//    def instantiated(params: Assignment): Y
//  }
//
//  class ParameterizedEffectToken[C, V](_parameters: Parameters,
//                                       fSV: Assignment => C,
//                                       fV: Assignment => V)
//      extends Parameterized[EffectToken[C, V]] {
//    override def parameters: Parameters = _parameters
//
//    override def instantiated(params: Assignment): EffectToken[C, V] =
//      EffectTokenImpl()
//  }
}
