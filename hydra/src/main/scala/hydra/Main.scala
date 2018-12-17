package hydra
import caseapp._
import dahu.planning.anml.planner.{Hybrid, HybridPlannerOptions}
import HybridPlannerOptions.instance._

object Main extends CaseApp[HybridPlannerOptions] {

  def run(command: HybridPlannerOptions, args: RemainingArgs): Unit = {

    Hybrid.run(command, args)

  }

}
