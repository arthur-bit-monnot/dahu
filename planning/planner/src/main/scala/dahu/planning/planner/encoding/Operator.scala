package dahu.planning.planner.encoding

import dahu.model.products.{FieldAccess, ProductTag}
import dahu.model.types._
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
  implicit val tag: ProductTag[OperatorF] = ProductTag.build[OperatorF](
    "operator",
    "name" -> Tag[String],
    "args" -> null, //Tag[Vec[Literal]],
    "start" -> Tag[Int],
    "end" -> Tag[Int],
    "depth" -> Tag[Int],
    "ins-lvl" -> Tag[Int],
    "id" -> Tag[Int],
    "first-dec-lvl" -> Tag[Int],
    "last-dec-lvl" -> Tag[Int]
  )

  val Name = tag.getAccessor[String]("name")
  //args
  val Start = tag.getAccessor[Int]("start")
  // end
  val Depth = tag.getAccessor[Int]("depth")
  val InsLvl = tag.getAccessor[Int]("ins-lvl")
  val Id = tag.getAccessor[Int]("id")
  val FirstDecLvl = tag.getAccessor[Int]("first-dec-lvl")
  val LastDecLvl = tag.getAccessor[Int]("last-dec-lvl")

}

case class SolutionF[F[_]](
    operators: F[Vec[Operator]],
    effects: F[Vec[EffTok]],
    conditions: F[Vec[CondTok]],
    continuousConditions: F[Vec[ContCondTok]]
)

object SolutionF {
  implicit val tag: ProductTag[SolutionF] = ProductTag.build[SolutionF](
    "planning.solution",
    "operators" -> Tag[Vec[Operator]],
    "effects" -> Tag[Vec[EffTok]],
    "conditions" -> Tag[Vec[CondTok]],
    "continuous-conditions" -> Tag[Vec[ContCondTok]]
  )

  val ContConds = tag.getAccessor[Vec[ContCondTok]]("continuous-conditions")

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
