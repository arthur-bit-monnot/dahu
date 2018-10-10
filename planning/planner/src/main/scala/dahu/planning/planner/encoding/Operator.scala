package dahu.planning.planner.encoding

import dahu.model.products.FieldAccess
import dahu.model.types.{ProductTag, Tag}
import dahu.utils.Vec
import spire.implicits._

case class OperatorF[F[_]](name: F[String],
                           args: F[Vec[Literal]],
                           start: F[Int],
                           end: F[Int],
                           depth: F[Int]) {
  override def toString: String = s"[$start, $end] $name($args)"
}
object OperatorF {
  val Name = FieldAccess[OperatorF, String]("name", 0)
  //args
  val Start = FieldAccess[OperatorF, Int]("start", 2)
  // end
  val Depth = FieldAccess[OperatorF, Int]("depth", 4)

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
