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

case class SolutionF[F[_]](
    operators: F[Vec[Operator]],
    effects: F[Vec[EffTok]]
)

object SolutionF {
  implicit val tag: ProductTag[SolutionF] = ProductTag.ofProd[SolutionF]
}

case class Plan(operators: Vec[Operator], effects: Vec[EffTok]) {
  import spire.implicits._
  def formatOperators: String = {
    operators.sortedBy(_.start).map(_.toString).toSeq.mkString("\n")
  }

  def formatEffects: String = {
    effects.sortedBy(e => (e.fluent.toString, e.startChange)).mkString("\n")
  }
}

object Plan {
  implicit val tag: Tag[Plan] = Tag.default[Plan]
}
