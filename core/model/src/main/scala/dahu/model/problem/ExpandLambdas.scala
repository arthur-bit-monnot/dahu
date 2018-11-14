package dahu.model.problem

import cats._
import dahu.graphs._
import dahu.graphs.transformations.{ManualTransformation, TransformationWithSubstitution}
import dahu.model.ir._
import dahu.utils._

object ExpandLambdas {

  def expandLambdas[K](graph: RootedASG[K, StaticF, Id]): RootedASG[K, Total, Id] = {
    import dahu.model.transformations._
    val firstOrderExpansions = makeOptimizer(Pass.allStaticPasses)
    val expandedFirstOrder = graph
      .postpro(makeOptimizer(totalPasses))
      .postpro(firstOrderExpansions)
      .postpro(firstOrderExpansions)
      .postpro(firstOrderExpansions)

    val x = expandedFirstOrder.fixID
    val y = x.tree.manualMap(PartialEval.asInstanceOf[ManualTransformation[StaticF, StaticF]])
    val z = y.rootedAt(graph.root)

//    API.echo(z)

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

            case x =>
              val y = x.smap(retrieve)
              val z = y.smap(_.smap(retrieve))
              println(x)
              println(y)
              println(z)
              dahu.utils.errors.unexpected
          }
        case x: Total[I] => (x, None)
      }
    }

    val expanded = z.transformWithSubstitution(trans)
    val opt = API.optimize(expanded)
    val opt2 = API.optimize(opt)

    opt2
  }

}
