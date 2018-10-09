package dahu.planning.planner.chronicles

import dahu.model.types.{ProductTag, Tag}
import dahu.planning.planner.hcsp.Literal
import dahu.utils.Vec

case class OperatorF[F[_]](name: F[String], args: F[Vec[Literal]], start: F[Int], end: F[Int]) {
  override def toString = s"[$start, $end] $name($args)"
}
object OperatorF {

  implicit val tag: ProductTag[OperatorF] = ProductTag.ofProd[OperatorF]
}

case class SolutionF[F[_]](operators: F[Vec[Operator]])

object SolutionF {
  implicit val tag: ProductTag[SolutionF] = ProductTag.ofProd[SolutionF]
}

case class Plan(operators: Vec[Operator])

object Plan {
  implicit val tag: Tag[Plan] = Tag.default[Plan]
}
