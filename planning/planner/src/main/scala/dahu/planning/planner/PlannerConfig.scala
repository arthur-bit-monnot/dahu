package dahu.planning.planner

sealed trait SymBreakLevel
object SymBreakLevel {
  case object No extends SymBreakLevel // No symetry breaking
  case object Base extends SymBreakLevel
}

case class PlannerConfig(minInstances: Int,
                         maxInstances: Int,
                         symBreak: SymBreakLevel = SymBreakLevel.No)
