package dahu.planning.planner

sealed trait SymBreakLevel
object SymBreakLevel {
  case object No extends SymBreakLevel // No symmetry breaking
  case object Base extends SymBreakLevel
  case object PlanSpace extends SymBreakLevel
}

case class PlannerConfig(minInstances: Int = 0,
                         maxInstances: Int = Int.MaxValue,
                         printDetailed: Boolean = false,
                         symBreak: SymBreakLevel = SymBreakLevel.PlanSpace)
