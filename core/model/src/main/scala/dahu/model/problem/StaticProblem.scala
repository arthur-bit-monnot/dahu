package dahu.model.problem

import java.util.Optional

import cats.Functor
import dahu.core.algebra.{BoolLike, GenBoolLike}
import dahu.model.input.Ident
import dahu.model.ir._
import dahu.model.math.bool
import dahu.recursion.FCoalgebra
import dahu.utils.{Bag, SFunctor, SubSubInt, Vec}
import dahu.utils.SFunctor._
import shapeless.tag.@@
import shapeless.the

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.reflect.ClassTag

object StaticProblem {

  class GenBoolLikeInstance[X: ClassTag]() extends GenBoolLike[X, Total[X]] {
    override def and(a: X, b: X): Total[X] = ComputationF(bool.And, Vec(a, b), bool.And.outType)
    override def or(a: X, b: X): Total[X] = ComputationF(bool.Or, Vec(a, b), bool.Or.outType)
    override def not(a: X): Total[X] = ComputationF(bool.Not, Vec(a), bool.Not.outType)
    override def False: Total[X] = bool.FalseF
    override def True: Total[X] = bool.TrueF
  }

  implicit def treeNodeStaticF(implicit instance: TreeNode[ExprF]): TreeNode[NoProviderF] =
    instance
      .asInstanceOf[TreeNode[NoProviderF]] // TODO: why doesn't contravariance work to provide this one

  def underClosedWorld[X](root: X,
                          coalgebra: FCoalgebra[ExprF, X]): LazyTree[X, StaticF, cats.Id, _] = {
    val lt = IlazyForest.build(coalgebra, algebra).fixID
    val provided = lt.getTreeRoot(root).provided.toSet.toSeq
//    val dynamics = lt.getTreeRoot(root).dynamics
    val ofValues = lt.mapExternal[cats.Id](_.value)
    val dynamicsErased = ofValues.mapInternalGen[StaticF](ctx => {
      _ match {
        case x: DynamicF[IDTop] =>
          val newProvided = provided.map(ctx.toNewId)
          val GBL: GenBoolLike[IDTop, Total[IDTop]] = new GenBoolLikeInstance[IDTop]
          val BL: BoolLike[IDTop] = GBL.asBoolLike(ctx.record)
          val trans = (id: IDTop) => BL.andN(newProvided: _*)
          provided.map(ctx.toNewId)
          val res = trans(x.params)
          val ret = ctx.retrieve(res)
          println(s"trans: $res $ret")
//          println(ret)
          ret
//          bool.TrueF
        case x: InputF[_] =>
          x
        case x: StaticF[IDTop] =>
          x
      }
    })
    LazyTree(dynamicsErased)(dynamicsErased.getTreeRoot(root))
  }

  case class IR[@specialized(Int) A](value: A, provided: Bag[A], dynamics: Bag[A])
  object IR {
    implicit val irFunctor: Functor[IR] = new Functor[IR] {
      override def map[A, B](fa: IR[A])(f: A => B): IR[B] =
        IR(f(fa.value), fa.provided.map(f), fa.dynamics.map(f))
    }
  }

  def getProvided(e: ExprF[IR[IDTop]]): Bag[IDTop] =
    Bag.fromIterables(TreeNode[ExprF].children(e).map(_.provided))
  def getDynamics(e: ExprF[IR[IDTop]]): Bag[IDTop] =
    Bag.fromIterables(TreeNode[ExprF].children(e).map(_.dynamics))

  def algebra(ctx: LazyForestGenerator.Context[NoProviderF, IDTop]): ExprF[IR[IDTop]] => IR[IDTop] = {
    case x: InputF[_] => IR(ctx.record(x), Bag.empty, Bag.empty)
    case x: CstF[_]   => IR(ctx.record(x), Bag.empty, Bag.empty)
    case x @ DynamicF(_, _, _, _) =>
      IR(
        value = ctx.record(x.smap(_.value)),
        provided = getProvided(x),
        dynamics = getDynamics(x) //TODO: do we need to track the dynamics
      )
    case x @ DynamicProviderF(v, provided, _) =>
      IR(
        value = v.value,
        provided = getProvided(x) + provided.value,
        dynamics = getDynamics(x)
      )
    case x @ OptionalF(value, present, typ) =>
      IR(
        value = ctx.record((x: StaticF[IR[IDTop]]).smap(_.value)),
        provided = getProvided(x).map { id: IDTop =>
          ctx.record(
            OptionalF(id, present.value, ctx.retrieve(id).typ)
          )
        },
        dynamics = getDynamics(x)
      )
    case x @ Partial(value, valid, typ) =>
      IR(
        value = ctx.record((x: StaticF[IR[IDTop]]).smap(_.value)),
        provided = getProvided(x),
//          .map { id: IDTop =>
//          ctx.record(Partial(id, valid.value, ctx.retrieve(id).typ))
//        },
        dynamics = getDynamics(x)
      )
    case x: StaticF[IR[IDTop]] =>
      assert(x.isInstanceOf[Total[_]] || x.isInstanceOf[ValidF[_]] || x.isInstanceOf[PresentF[_]])
      IR(
        value = ctx.record(SFunctor[StaticF].smap(x)(_.value)),
        provided = getProvided(x),
        dynamics = getDynamics(x)
      )
  }

}
