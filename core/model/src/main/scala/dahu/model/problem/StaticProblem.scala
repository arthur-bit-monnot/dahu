package dahu.model.problem

import cats.{Functor, Id}
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.model.ir._
import dahu.model.math.bool
import dahu.recursion.FCoalgebra
import dahu.utils.{Bag, SFunctor, Vec}
import dahu.utils.SFunctor._

object StaticProblem {

  /** Denotes an export of in a given problem.
    *
    * @param value Structure that is exported.
    * @param context Scope of the export, mapping to a boolean variable.
    * @tparam X
    */
  case class Export[X](value: X, context: X)

  def closeTheWorld[X](t: RootedASG[X, ExprF, cats.Id],
                       provided: Seq[Export[X]]): RootedASG[X, StaticF, cats.Id] = {
    val openAsg = t.fixID

    val providedInternalized = provided.map {
      case Export(x, y) => Export(openAsg.tree.getTreeRoot(x), openAsg.tree.getTreeRoot(y))
    }
    closeTheWorldIntern(openAsg, providedInternalized)
  }

  def closeTheWorldIntern[X, I <: IDTop](
      t: LazyTree[X, ExprF, cats.Id, I],
      provided: Seq[Export[I]]): LazyTree[X, StaticF, cats.Id, _] = {

    val dynamicsErased: OpenASG[X, StaticF, cats.Id, _] =
      t.tree
        .mapInternalGen[StaticF](ctx => {
          val rec = ctx.record
          _ match {
            case x: DynamicF[IDTop] =>
              val default = rec(x.monoid.liftedIdentity) // TODO (optim): cache out of the loop

              // TODO: filter should not use type equality but a more general type intersection mechanism
              val filter: IDTop => Boolean = {
                x.accept match {
                  case Some(f) =>
                    (i: IDTop) =>
                      {
                        val t = ctx.retrieve(i).typ
                        (t intersects x.acceptedType) && f(t)
                      }
                  case None =>
                    (i: IDTop) =>
                      x.acceptedType intersects ctx.retrieve(i).typ
                }
              }
//              val newProvided = provided.map(ctx.toNewId).withFilter(filter)
              // TODO (optim): cache out of the loop (with WeakRef?)
              val allNewProvided: Seq[(IDTop, IDTop)] = provided.map {
                case Export(exp, scope) => (ctx.toNewId(exp), ctx.toNewId(scope))
              }
              val newProvided = allNewProvided.withFilter { case (i, p) => filter(i) }
              val lbd = ctx.retrieve(x.f)
              def compute(oneProvided: IDTop): IDTop =
                ctx.record(ApplyF(x.f, oneProvided, lbd.typ))

              val conditionals: Seq[IDTop] =
                newProvided.map {
                  case (i, prez) =>
                    rec(ITEF[IDTop](prez, compute(i), default, x.monoid.tpe))
                }

              val value = ComputationF[IDTop](x.monoid, conditionals, x.monoid.tpe)
              value
            case x: InputF[_] =>
              x
            case x: StaticF[IDTop] =>
              x
          }
        })

    LazyTree(dynamicsErased.fixID)(t.root)
  }

}
