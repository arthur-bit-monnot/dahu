package dahu.model.problem

import dahu.graphs._
import dahu.model.ir._
import dahu.model.products.ProductTagAny
import dahu.model.structs.OptionalF
import dahu.model.types.SequenceTag.SequenceTagImplAny
import dahu.utils.Vec

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
            case DynCollectorF(collectedType, optFilter, _) =>
              val filter: IDTop => Boolean = {
                optFilter match {
                  case Some(f) =>
                    (i: IDTop) =>
                      {
                        val t = ctx.retrieve(i).typ
                        (t intersects collectedType) && f(t)
                      }
                  case None =>
                    (i: IDTop) =>
                      collectedType intersects ctx.retrieve(i).typ
                }
              }
              // TODO (optim): cache out of the loop (with WeakRef?)
              val allNewProvided: Seq[(IDTop, IDTop)] = provided.map {
                case Export(exp, scope) => (ctx.toNewId(exp), ctx.toNewId(scope))
              }
              val optionalTag: ProductTagAny = OptionalF.tagOfAny(collectedType)
              val newProvided = allNewProvided.filter { case (i, p) => filter(i) }
              val optionalProvided: Seq[IDTop] = newProvided.map {
                case (i, p) => ctx.record(ProductF[IDTop](Vec(i, p), optionalTag))
              }
              SequenceF[IDTop](Vec.fromSeq(optionalProvided), SequenceTagImplAny(optionalTag))

            case x: InputF[_] =>
              x
            case x: StaticF[IDTop] =>
              x
          }
        })

    LazyTree(dynamicsErased.fixID)(t.root)
  }

}
