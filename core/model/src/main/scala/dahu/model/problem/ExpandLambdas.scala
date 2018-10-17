package dahu.model.problem

import cats._
import dahu.graphs._
import dahu.graphs.transformations.TransformationWithSubstitution
import dahu.model.ir._

object ExpandLambdas {

  def expandLambdas[K](graph: RootedASG[K, StaticF, Id]): RootedASG[K, Total, Id] = {
    import dahu.model.transformations._
    val firstOrderExpansions = makeOptimizer(Pass.allStaticPasses)
    val expandedFirstOrder = graph.postpro(makeOptimizer(totalPasses)).postpro(firstOrderExpansions)

    val trans = new TransformationWithSubstitution[StaticF, Total] {
      override def transformation[I <: Int](
          retrieve: I => Total[I]): StaticF[I] => (Total[I], Option[I => I]) = {
        case ApplyF(lbd, param, _) =>
          // lambda application, return the tree of the lambda with a substitution that replaces the
          // param of the lambda by the target of the application
          retrieve(lbd) match {
            case dahu.model.ir.LambdaF(in, tree, _, _) =>
              val rt = retrieve(tree)
              (rt, Some(id => if(id == in) param else id))

            case _ => dahu.utils.errors.unexpected
          }
        case x: Total[I] => (x, None)
      }
    }

    val expanded = expandedFirstOrder.transformWithSubstitution(trans)
    val opt = API.optimize(expanded)
    val opt2 = API.optimize(opt)

    opt2
  }

}
