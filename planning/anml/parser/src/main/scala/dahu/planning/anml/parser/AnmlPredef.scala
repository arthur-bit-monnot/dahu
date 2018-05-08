package dahu.planning.anml.parser

import dahu.planning.model.common._
import dahu.planning.model.common.Type._
import dahu.planning.model.full
import dahu.planning.model.full._

object AnmlPredef extends dahu.planning.model.common.Predef {

  val StartSym = "start"
  val EndSym = "end"
  val TrueSym = "true"
  val FalseSym = "false"
  val TimeSym = "time"
  val BooleanSym = "boolean"

  override val Time: IRealType = IntSubType(RootScope / TimeSym, Integers)

  override val Boolean: BooleanType = BooleanType(RootScope / BooleanSym)

  override val True: Instance = Instance(RootScope / TrueSym, Boolean)
  override val False: Instance = Instance(RootScope / FalseSym, Boolean)

  override val Start = LocalVar(RootScope / StartSym, Time)
  override val End = LocalVar(RootScope / EndSym, Time)

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
