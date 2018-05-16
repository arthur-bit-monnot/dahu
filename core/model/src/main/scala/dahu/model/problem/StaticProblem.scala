package dahu.model.problem

import java.util.Optional

import cats.Functor
import dahu.core.algebra.{BoolLike, GenBoolLike}
import dahu.model.input.Ident
import dahu.model.ir._
import dahu.recursion.FCoalgebra
import dahu.utils.{Bag, SFunctor, Vec}
import dahu.utils.SFunctor._
import shapeless.tag.@@
import shapeless.the

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.reflect.ClassTag

object StaticProblem {

  private def trans(provided: Iterable[ID])(ctx: Context[StaticF]): DynamicF[ID] => StaticF[ID] =
    ???

  def underClosedWorld[X](root: X,
                          coalgebra: FCoalgebra[ExprF, X]): LazyTree[X, StaticF, cats.Id] = {
    val lt: IlazyForest[X, StaticF, IR] = new LazyForestGenerator(coalgebra, algebra)
    val provided = lt.getTreeRoot(root).provided
    val dynamics = lt.getTreeRoot(root).dynamics
    val ofValues = lt.mapExternal[cats.Id](_.value)
    val dynamicsErased = ofValues.mapInternal[StaticF] {
      case x @ InputF(Ident.Provided(DynamicF(param, dyn, _)), _) =>
        param
        println(x)
        x
      case x: InputF[_] =>
        println(x)
        x
      case x =>
        println(x)
        x
    }
    LazyTree(dynamicsErased.getTreeRoot(root), dynamicsErased)
  }

  case class IR[@specialized(Int) A](value: A, provided: Bag[A], dynamics: Bag[A])
  object IR {
    implicit val irFunctor: Functor[IR] = new Functor[IR] {
      override def map[A, B](fa: IR[A])(f: A => B): IR[B] =
        IR(f(fa.value), fa.provided.map(f), fa.dynamics.map(f))
    }
  }

  def getProvided(e: ExprF[IR[ID]]): Bag[ID] =
    Bag.fromIterables(TreeNode[ExprF].children(e).map(_.provided))
  def getDynamics(e: ExprF[IR[ID]]): Bag[ID] =
    Bag.fromIterables(TreeNode[ExprF].children(e).map(_.dynamics))

  def algebra(ctx: Context[StaticF]): ExprF[IR[ID]] => IR[ID] = {
    case x: InputF[_] => IR(ctx.rec(x), Bag.empty, Bag.empty)
    case x: CstF[_]   => IR(ctx.rec(x), Bag.empty, Bag.empty)
    case x @ DynamicF(params, instantiator, tpe) =>
      val id = ctx.rec(InputF(Ident(x.smap(_.value)), tpe))
      val ir = IR(
        value = id,
        provided = getProvided(x),
        dynamics = getDynamics(x) + id
      )
      ir
    case x @ DynamicProviderF(v, provided, _) =>
      IR(
        value = v.value,
        provided = getProvided(x) + provided.value,
        dynamics = getDynamics(x)
      )
    case x @ OptionalF(value, present, typ) =>
      IR(
        value = ctx.rec((x: StaticF[IR[ID]]).smap(_.value)),
        provided = getProvided(x).map { id: ID =>
          ctx.rec(
            OptionalF(id, present.value, ctx.retrieve(id).typ)
          )
        },
        dynamics = getDynamics(x)
      )
    case x @ Partial(value, valid, typ) =>
      IR(
        value = ctx.rec((x: StaticF[IR[ID]]).smap(_.value)),
        provided = getProvided(x).map { id: ID =>
          ctx.rec(Partial(id, valid.value, ctx.retrieve(id).typ))
        },
        dynamics = getDynamics(x)
      )
    case x: StaticF[IR[ID]] =>
      assert(x.isInstanceOf[Total[_]] || x.isInstanceOf[ValidF[_]] || x.isInstanceOf[PresentF[_]])
      IR(
        value = ctx.rec(SFunctor[StaticF].smap(x)(_.value)),
        provided = getProvided(x),
        dynamics = getDynamics(x)
      )
  }

}
