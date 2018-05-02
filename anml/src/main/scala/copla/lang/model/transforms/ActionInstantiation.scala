package copla.lang.model.transforms

import copla.lang.model.common._
import copla.lang.model.core._
import landscaper.transformations._

object ActionInstantiation {

  private[this] object implicits {
    type Up[T] = Trans.Aux[Id, Id, T, T]
    def instance[T](implicit ev: Up[T]): Up[T] = ev

//    // landscaper has problem with the new ADT, provide some help
//    implicit val x3 = instance[IntExpr]
//    implicit val x4 = instance[TPRef]
//    implicit val x5 = instance[TBefore] // ok
//    implicit val x6 = instance[StaticAssertion] //ok
//    implicit val x7 = instance[ArgDeclaration] //ok
//    implicit val x8 = instance[TimedAssignmentAssertion] //ok
//    implicit val x9 = instance[TimedEqualAssertion] //ok
//    implicit val x10 = instance[TimedTransitionAssertion] //ok
//    implicit val x11 = instance[TimedAssertion] //ok
//    implicit val x12 = instance[LocalVarDeclaration] //ok
//    implicit val x13 = instance[TimepointDeclaration] //ok
//
//    // derivation of Trans[Id,Id,InActionBlock] never ends for an obscure reason, dispatch manually
//    implicit val main: Up[InActionBlock] = new Trans[Id, Id, InActionBlock] {
//      override type Result = InActionBlock
//
//      override def rewrite(f: Id => Id, in: InActionBlock): Result = in match {
//        case x: TimedAssertion       => x11.rewrite(f, x)
//        case x: StaticAssertion      => x6.rewrite(f, x)
//        case x: TBefore              => x5.rewrite(f, x)
//        case x: ArgDeclaration       => x7.rewrite(f, x)
//        case x: LocalVarDeclaration  => x12.rewrite(f, x)
//        case x: TimepointDeclaration => x13.rewrite(f, x)
//      }
//    }
  }

  /** Builds a new action instance with the given name*/
  def instance(template: ActionTemplate, instanceName: String): Action = {
    val instanceScope: InnerScope = template.scope.parent + instanceName

    val trans: Id => Id = x => {
      var transScope: Scope => Scope = null
      transScope = {
        case s if s == template.scope => template.scope.parent + instanceName
        case RootScope                => RootScope
        case InnerScope(s, name)      => transScope(s) + name
      }
      x match { case Id(s, name) => Id(transScope(s), name) }
    }

//
    ???
//    val instanceContent = template.content.map(s => implicits.main.rewrite(trans, s))
//    Action(instanceScope, instanceContent, template)
  }
}
