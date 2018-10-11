package dahu.planning.planner.encoding

import dahu.model.products.FieldAccess
import dahu.model.types.{ProductTag, Tag}
import dahu.utils.Vec
import spire.implicits._

case class OperatorF[F[_]](name: F[String],
                           args: F[Vec[Literal]],
                           start: F[Int],
                           end: F[Int],
                           depth: F[Int],
                           insertionLvl: F[Int],
                           id: F[Int],
                           firstDecisionLevel: F[Int],
                           lastDecisionLevel: F[Int]) {
  override def toString: String =
    s"[$start, $end] $name($args) -- depth:$depth -- ins-lvl:$insertionLvl -- id:$id -- fst-dl:$firstDecisionLevel -- lst-dl:$lastDecisionLevel"
}
object OperatorF {
  implicit val tag: ProductTag[OperatorF] = ProductTag.ofProd[OperatorF]

  val Name = FieldAccess[OperatorF, String]("name", 0)
  //args
  val Start = FieldAccess[OperatorF, Int]("start", 2)
  // end
  val Depth = FieldAccess[OperatorF, Int]("depth", 4)
  val InsLvl = FieldAccess[OperatorF, Int]("ins-lvl", 5)
  val Id = FieldAccess[OperatorF, Int]("id", 6)
  val FirstDecLvl = FieldAccess[OperatorF, Int]("first-dec-lvl", 7)
  val LastDecLvl = FieldAccess[OperatorF, Int]("last-dec-lvl", 8)

}

case class SolutionF[F[_]](
    operators: F[Vec[Operator]],
    effects: F[Vec[EffTok]],
    conditions: F[Vec[CondTok]]
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
