package dahu.planning.pddl

import dahu.planning.model.common._
import dahu.planning.model.common.Type._
import dahu.planning.model.full._

case class PddlPredef(discretization: Int) extends Predef {

  val StartSym = "⊢"
  val EndSym = "⊣"
  val TrueSym = "⊤"
  val FalseSym = "⊥"
  val TimeSym = "𝓣𝓲𝓶𝓮"
  val BooleanSym = "𝓑𝓸𝓸𝓵"

  override val Time: IRealType = IntSubType(RootScope / TimeSym, Integers)

  override val Boolean: BooleanType = BooleanType(RootScope / BooleanSym)

  override val True: Instance = Instance(RootScope / TrueSym, Boolean)
  override val False: Instance = Instance(RootScope / FalseSym, Boolean)

  override val Start = LocalVar(RootScope / StartSym, Time)
  override val End = LocalVar(RootScope / EndSym, Time)

  val Number = IntSubType(RootScope / "number", Integers)

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
