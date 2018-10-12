package dahu.planning.planner

import caseapp.HelpMessage
import caseapp.core.argparser.ArgParser

sealed trait SymBreakLevel
object SymBreakLevel {
  case object No extends SymBreakLevel // No symmetry breaking
  case object Base extends SymBreakLevel
  case object PlanSpace extends SymBreakLevel

  implicit val customArgParser: ArgParser[SymBreakLevel] = new ArgParser[SymBreakLevel] {
    override def apply(current: Option[SymBreakLevel],
                       value: String): Either[caseapp.core.Error, SymBreakLevel] = value match {
      case "no" | "0"         => Right(No)
      case "base" | "1"       => Right(Base)
      case "plan-space" | "2" => Right(PlanSpace)
      case _                  => Left(caseapp.core.Error.UnrecognizedValue(value))
    }
    override def description: String = "Choose which level of symmetry breaking you want to use."
  }
}

case class PlannerConfig(
    @HelpMessage("Minimal depth to start with")
    minInstances: Int = 0,
    @HelpMessage("Maximal depth ")
    maxInstances: Int = Int.MaxValue,
    @HelpMessage(
      "Print the detailed result of planning, exposing some internal variables of the system.")
    printDetailed: Boolean = false,
    symBreak: SymBreakLevel = SymBreakLevel.Base)
