package dahu.planning.anml.parser

import dahu.planning.model.common._
import dahu.planning.model.common.Type._
import dahu.planning.model.full
import dahu.planning.model.full._

object AnmlPredef extends dahu.planning.model.common.Predef {
  override val Time: IRealType = IntSubType(Id(RootScope, "time"), Integers)

  override val Boolean: BooleanType = BooleanType(Id(RootScope, "boolean"))

  override val True: Instance = Instance(Id(RootScope, "true"), Boolean)
  override val False: Instance = Instance(Id(RootScope, "false"), Boolean)

  override val Start = LocalVar(Id(RootScope, "start"), Time)
  override val End = LocalVar(Id(RootScope, "end"), Time)

  override def baseModel: full.Model =
    (Model() ++ Seq(
      TypeDeclaration(ObjectTop),
      TypeDeclaration(Boolean),
      TypeDeclaration(Reals),
      TypeDeclaration(Integers),
      InstanceDeclaration(True),
      InstanceDeclaration(False),
      LocalVarDeclaration(Start),
      LocalVarDeclaration(End),
    )).getOrElse(sys.error("Could not instantiate base model"))
}
