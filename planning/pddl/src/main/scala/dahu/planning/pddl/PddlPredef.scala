package dahu.planning.pddl

import dahu.planning.model.common._
import dahu.planning.model.common.Type._
import dahu.planning.model.full._

object PddlPredef extends Predef {
  private val scope = RootScope + "_predef_"
  override val Time: IRealType = IntSubType(scope / "time", Integers)

  override val Boolean: BooleanType = BooleanType(scope / "boolean")

  override val True: Instance = Instance(scope / "true", Boolean)
  override val False: Instance = Instance(scope / "false", Boolean)

  override val Start = LocalVar(scope / "start", Time)
  override val End = LocalVar(scope / "end", Time)

  val Number = IntSubType(RootScope / "number", Integers)

  val discretization = 1000
  def discretize(d: Double): Int = (d * discretization).toInt

  override def baseModel: Model =
    (Model() ++ Seq(
      TypeDeclaration(ObjectTop),
      TypeDeclaration(Boolean),
      TypeDeclaration(Reals),
      TypeDeclaration(Integers),
      TypeDeclaration(Time),
      TypeDeclaration(Number),
      InstanceDeclaration(True),
      InstanceDeclaration(False),
      LocalVarDeclaration(Start),
      LocalVarDeclaration(End),
    )).getOrElse(sys.error("Could not instantiate base model"))
}
