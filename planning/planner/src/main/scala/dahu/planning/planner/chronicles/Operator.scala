package dahu.planning.planner.chronicles

import dahu.model.types.{ProductTag, Tag}
import dahu.planning.planner.Literal
import dahu.utils.Vec

case class OperatorF[F[_]](name: F[String], args: F[Vec[Literal]], start: F[Int], end: F[Int]) {
  override def toString = s"[$start, $end] $name($args)"
}
object OperatorF {

  implicit val tag: ProductTag[OperatorF] = ProductTag[OperatorF]
}

case class Plan(operators: Vec[Operator])

object Plan {
  implicit val tag: Tag[Plan] = Tag.default[Plan]
}
