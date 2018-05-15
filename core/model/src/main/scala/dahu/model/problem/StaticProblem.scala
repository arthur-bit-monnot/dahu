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

  def underClosedWorld[X <: Int](root: X,
                                 coalgebra: X => ExprF[X]): LazyTree[X, StaticF, cats.Id] = {
    encode(root, coalgebra)
  }

  def encode[X <: Int](root: X, coalgebra: FCoalgebra[ExprF, X]): LazyTree[X, StaticF, cats.Id] = {
    val lt: IlazyForest[X, StaticF, IR] = new LazyForestGenerator(coalgebra, algebra)

    // TODO: erase the dynamics
//
//    val totalTrees = new ILazyTree[X, Total, cats.Id] {
//      override def getExt(k: X): Total[ID] = lt.get(lt.get(k).value)
//      override def getInt(i: ID): Total[ID] = lt.get(i)
//      override def getInternalID(k: X): ID = lt.get(k).value
//    }
//    val satRoot = lt.get(root).valid
//    RootedLazyTree(satRoot, totalTrees)
    ???
  }

  case class IR[@specialized(Int) A](value: A, provided: Bag[A], dynamics: Bag[A])
  object IR {
    implicit val irFunctor: Functor[IR] = new Functor[IR] {
      override def map[A, B](fa: IR[A])(f: A => B): IR[B] =
        IR(f(fa.value), fa.provided.map(f), fa.dynamics.map(f))
    }
  }
  def childrenFlatten[A](e: ExprF[Bag[A]]): Bag[A] = ???
  def getProvided(e: ExprF[IR[ID]]): Bag[ID] = childrenFlatten(e.smap(_.provided))
  def getDynamics(e: ExprF[IR[ID]]): Bag[ID] = childrenFlatten(e.smap(_.dynamics))

  def algebra(ctx: Context[StaticF]): ExprF[IR[ID]] => IR[ID] = {
    case x: InputF[_] => IR(ctx.rec(x), Bag.empty, Bag.empty)
    case x: CstF[_]   => IR(ctx.rec(x), Bag.empty, Bag.empty)
    case x @ DynamicF(params, instantiator, tpe) =>
      val id = ctx.rec(InputF(Ident(x.smap(_.value)), tpe))
      IR(
        value = id,
        provided = getProvided(x),
        dynamics = getDynamics(x) + id
      )
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
