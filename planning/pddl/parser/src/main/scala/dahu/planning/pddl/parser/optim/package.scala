package dahu.planning.pddl.parser

import dahu.planning.model.common.Predef
import dahu.planning.model.core.CoreModel

import scala.util.Try

package object optim {

  def reorderTimelines(model: CoreModel)(implicit options: Options,
                                         predef: Predef): Try[CoreModel] =
    new ActionRewrite(options).optimize(model)
  def inferInvariants(model: CoreModel)(implicit predef: Predef): Try[CoreModel] =
    Try(new InvariantInference(model).rewritten)

  def all(model: CoreModel)(implicit predef: Predef, option: Options): Try[CoreModel] =
    for {
      m1 <- reorderTimelines(model)
      m2 <- inferInvariants(m1)
    } yield m2

}
