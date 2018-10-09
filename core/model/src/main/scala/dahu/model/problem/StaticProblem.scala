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

  def closeTheWorld[X, I <: IDTop](t: LazyTree[X, ExprF, cats.Id, I],
                                   provided: Seq[(X, X)]): LazyTree[X, StaticF, cats.Id, _] = {

    val providedInternalized = provided.map {
      case (x, y) => (t.tree.getTreeRoot(x), t.tree.getTreeRoot(y))
    }
    closeTheWorldIntern(t, providedInternalized)
  }

  def closeTheWorldIntern[X, I <: IDTop](
      t: LazyTree[X, ExprF, cats.Id, I],
      provided: Seq[(I, I)]): LazyTree[X, StaticF, cats.Id, _] = {

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
                case (exp, scope) => (ctx.toNewId(exp), ctx.toNewId(scope))
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

  // TODO: we could keep the cats.Id parameter generic
  def underClosedWorld[X](
      _tree: LazyTree[X, ExprF, cats.Id, _]): LazyTree[X, StaticF, cats.Id, _] = {
    val tree = _tree.fixID
    val internalRoot = tree.tree.getTreeRoot(tree.root)
    val lowLevelTree =
      underClosedWorldBase[tree.ID](internalRoot, i => tree.tree.internalCoalgebra(i)).fixID
    val finalTree: OpenASG[X, StaticF, Id, lowLevelTree.tree.ID] =
      lowLevelTree.tree.changedKey[X](x => tree.tree.getTreeRoot(x))
    LazyTree[X, StaticF, Id, finalTree.ID](finalTree)(tree.root)
  }

  def underClosedWorldBase[X](root: X,
                              coalgebra: FCoalgebra[ExprF, X]): LazyTree[X, StaticF, cats.Id, _] = {
    val lt = OpenASG.build(coalgebra)(algebra).fixID
    val provided = lt.getTreeRoot(root).provided.toSeq.distinct
//    val TMP = provided.toList.map(lt.internalCoalgebra)
//    println(TMP.map(_.typ))
    val ofValues = lt.mapExternal[cats.Id](_.value)
    val dynamicsErased: OpenASG[X, StaticF, cats.Id, _] =
      ofValues
        .mapInternalGen[StaticF](ctx => {
          val rec = ctx.record
          _ match {
            case x: DynamicF[IDTop] =>
              val default = rec(x.monoid.liftedIdentity)
              val filter: IDTop => Boolean = {
                x.accept match {
                  case Some(f) =>
                    (i: IDTop) =>
                      f(ctx.retrieve(i).typ)
                  case None =>
                    _ =>
                      true
                }
              }
              val newProvided = provided.map(ctx.toNewId).withFilter(filter)
              val lbd = ctx.retrieve(x.f)
              def compute(oneProvided: IDTop): IDTop =
                ctx.record(ApplyF(x.f, oneProvided, lbd.typ))

              val conditionals: Vec[IDTop] = ??? // TODO: (pure)
//                Vec(newProvided.map(i =>
//                  rec(ITEF[IDTop](rec(PresentF(i)), compute(i), default, x.monoid.tpe))): _*)

              val value = ComputationF[IDTop](x.monoid, conditionals, x.monoid.tpe)
              value
            case x: InputF[_] =>
              x
            case x: StaticF[IDTop] =>
              x
          }
        })

    LazyTree(dynamicsErased.fixID)(root)
  }

  case class IR[@specialized(Int) A](value: A, provided: Bag[A])
  object IR {
    implicit val irFunctor: Functor[IR] = new Functor[IR] {
      override def map[A, B](fa: IR[A])(f: A => B): IR[B] =
        IR(f(fa.value), fa.provided.map(f))
    }
  }

  def getProvided(e: ExprF[IR[IDTop]]): Bag[IDTop] =
    Bag.fromIterables(TreeNode[ExprF].children(e).map(_.provided))

  def algebra(ctx: LazyForestGenerator.Context[ExprF, IDTop]): ExprF[IR[IDTop]] => IR[IDTop] = {
    case x: InputF[_] => IR(ctx.record(x), Bag.empty)
    case x: CstF[_]   => IR(ctx.record(x), Bag.empty)
    case x @ DynamicF(_, _, _, _, _) =>
      IR(
        value = ctx.record(x.smap(_.value)),
        provided = getProvided(x)
      )
    case x: StaticF[IR[IDTop]] =>
      IR(
        value = ctx.record(SFunctor[StaticF].smap(x)(_.value)),
        provided = getProvided(x)
      )
  }

}
