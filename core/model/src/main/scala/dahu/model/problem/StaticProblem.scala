package dahu.model.problem

import cats.{Functor, Id}
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.model.ir._
import dahu.recursion.FCoalgebra
import dahu.utils.{Bag, SFunctor, Vec}
import dahu.utils.SFunctor._

object StaticProblem {

  // TODO: we could keep the cats.Id parameter generic
  def underClosedWorld[X](
      _tree: LazyTree[X, ExprF, cats.Id, _]): LazyTree[X, StaticF, cats.Id, _] = {
    val tree = _tree.fixID
    val internalRoot = tree.tree.getTreeRoot(tree.root)
    val lowLevelTree =
      underClosedWorldBase[tree.ID](internalRoot, i => tree.tree.internalCoalgebra(i)).fixID
    val finalTree: IlazyForest[X, StaticF, Id, lowLevelTree.tree.ID] =
      lowLevelTree.tree.changedKey[X](x => tree.tree.getTreeRoot(x))
    LazyTree[X, StaticF, Id, finalTree.ID](finalTree)(tree.root)
  }

  def underClosedWorldBase[X](root: X,
                              coalgebra: FCoalgebra[ExprF, X]): LazyTree[X, StaticF, cats.Id, _] = {
    val lt = IlazyForest.build(coalgebra, algebra).fixID
    val provided = lt.getTreeRoot(root).provided.toSeq.distinct
//    val TMP = provided.toList.map(lt.internalCoalgebra)
//    println(TMP.map(_.typ))
    val ofValues = lt.mapExternal[cats.Id](_.value)
    val dynamicsErased =
      ofValues.mapInternalGen[StaticF](ctx => {
        val rec = ctx.record
        _ match {
          case x: DynamicF[IDTop] =>
            val default = rec(x.monoid.liftedIdentity)
            val newProvided = provided.map(ctx.toNewId)
            val lbd = ctx.retrieve(x.f)
            def compute(oneProvided: IDTop): IDTop =
              ctx.record(ApplyF(x.f, oneProvided, lbd.typ))

            val conditionals: Vec[IDTop] =
              Vec(newProvided.map(i =>
                rec(ITEF[IDTop](rec(PresentF(i)), compute(i), default, x.monoid.tpe))): _*)

            val value = ComputationF[IDTop](x.monoid, conditionals, x.monoid.tpe)
            value
          case x: InputF[_] =>
            x
          case x: StaticF[IDTop] =>
            x
        }
      })
    LazyTree(dynamicsErased)(root)
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

  def algebra(ctx: LazyForestGenerator.Context[NoProviderF, IDTop]): ExprF[IR[IDTop]] => IR[IDTop] = {
    case x: InputF[_] => IR(ctx.record(x), Bag.empty)
    case x: CstF[_]   => IR(ctx.record(x), Bag.empty)
    case x @ DynamicF(_, _, _) =>
      IR(
        value = ctx.record(x.smap(_.value)),
        provided = getProvided(x)
      )
    case x @ DynamicProviderF(v, provided, _) =>
      IR(
        value = v.value,
        provided = getProvided(x) + provided.value
      )
    case x @ OptionalF(value, present, typ) =>
      IR(
        value = ctx.record((x: StaticF[IR[IDTop]]).smap(_.value)),
        provided = getProvided(x)
          .map { id: IDTop =>
            ctx.record(
              OptionalF(id, present.value, ctx.retrieve(id).typ)
            )
          }
      )
    case x @ Partial(value, valid, typ) =>
      IR(
        value = ctx.record((x: StaticF[IR[IDTop]]).smap(_.value)),
        provided = getProvided(x)
      )
    case x: StaticF[IR[IDTop]] =>
      IR(
        value = ctx.record(SFunctor[StaticF].smap(x)(_.value)),
        provided = getProvided(x)
      )
  }

}
