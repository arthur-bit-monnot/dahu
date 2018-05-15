package dahu.model.problem

import java.util.Optional

import dahu.core.algebra.{BoolLike, GenBoolLike}
import dahu.model.input.Ident
import dahu.model.ir._
import dahu.model.problem.SatisfactionProblem.RootedLazyTree
import dahu.recursion.FCoalgebra
import dahu.utils.{Bag, SFunctor, Vec}
import dahu.utils.SFunctor._
import shapeless.tag.@@
import shapeless.the

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.reflect.ClassTag

object StaticProblem {

  def satisfactionSubAST(ast: AST[_]): RootedLazyTree[ast.ID, StaticF, cats.Id] = {
//    encode(ast.root, ast.tree.asFunction)
    ???
  }

  def encode[@specialized(Int) X](root: X,
                                  coalgebra: FCoalgebra[ExprF, X],
                                  optimize: Boolean = true): RootedLazyTree[X, Total, cats.Id] = {
//    val lt = new LazyTreeSpec[X](coalgebra, compiler)
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

  class BoolLikeImpl[ID <: Int, M[_]](rec: M[ID] => ID, B: GenBoolLike[ID, M[ID]])
      extends BoolLike[ID] {
    override def and(a: ID, b: ID): ID = rec(B.and(a, b))
    override def or(a: ID, b: ID): ID = rec(B.or(a, b))
    override def not(a: ID): ID = rec(B.not(a))
    override val False: ID = rec(B.False)
    override val True: ID = rec(B.True)
  }

  final class Context[M[_]](val directRec: M[ID] => ID, val retrieve: ID => M[ID])(
      implicit B: GenBoolLike[ID, M[ID]]) {
    val X: BoolLike[ID] = new BoolLikeImpl[ID, M](rec, B)
    def rec(expr: M[ID]): ID = directRec(expr)
//      directRec(Optimizations.optimizer.optim(retrieve, directRec)(expr))

    def TRUE = X.True
    def FALSE = X.False
    def and(conjuncts: ID*): ID = X.andN(conjuncts: _*) // and(Vec.unsafe(conjuncts.toArray))
//    def and(conjuncts: Vec[ID]): ID = ??? //rec(ComputationF(bool.And, conjuncts, Tag.ofBoolean))
    def or(disjuncts: ID*): ID = X.orN(disjuncts: _*)
//      rec(ComputationF(bool.Or, Vec.unsafe(disjuncts.toArray), Tag.ofBoolean))
    def not(e: ID): ID = ???
//      rec(ComputationF(bool.Not, Seq(e), Tag.ofBoolean))

    def implies(cond: ID, eff: ID): ID =
      or(not(cond), eff)

  }
  trait Mark
  type ID = Int @@ Mark
  implicit val ct: ClassTag[ID] = ClassTag.Int.asInstanceOf[ClassTag[ID]]
  // F = ExprF
  // G = IR
  // H = Total
  class LazyTreeSpec[@specialized(Int) K, F[_]: TreeNode: SFunctor, G[_], H[_]](
      f: K => F[K],
      g: Context[H] => F[G[ID]] => G[ID])(implicit B: GenBoolLike[ID, H[ID]], ct: ClassTag[G[ID]]) {
//    private val treeNode = implicitly[TreeNode[F]]
//    private val functor = implicitly[SFunctor[F]]

    private val idsMap = mutable.HashMap[K, G[ID]]()
    private val repMap = mutable.ArrayBuffer[H[ID]]() // ID => H[ID]
    private val memo = mutable.HashMap[H[ID], ID]()

    private def getID(e: H[ID]): ID = {
      if(memo.contains(e))
        memo(e)
      else {
        val id = repMap.size.asInstanceOf[ID]
        repMap += e
        memo += ((e, id))
        id
      }
    }
    private val ctx: Context[H] = new Context(getID, get)

    private val g2: F[G[ID]] => G[ID] = g(ctx)

    @inline private def processed(k: K): Boolean = idsMap.contains(k)

    def get(i: ID): H[ID] = repMap(i)

    def get(key: K): G[ID] = {
      val queue = mutable.Stack[K]()
      queue.push(key)

      while(queue.nonEmpty) {
        val cur = queue.pop()
        val fk = f(cur)
        if(the[TreeNode[F]].children(fk).forall(processed)) {
          val fg = the[SFunctor[F]].smap(fk)(idsMap)
          val g: G[ID] = g2(fg)
          idsMap += ((cur, g))
        } else {
          queue.push(cur)
          queue.pushAll(the[TreeNode[F]].children(fk))
        }
      }
      idsMap(key)
    }
  }
  case class IR[@specialized(Int) A](value: A, provided: Bag[A], dynamics: Bag[A])

  def childrenFlatten[A](e: ExprF[Bag[A]]): Bag[A] = ???
  def getProvided(e: ExprF[IR[ID]]): Bag[ID] = childrenFlatten(e.smap(_.provided))
  def getDynamics(e: ExprF[IR[ID]]): Bag[ID] = childrenFlatten(e.smap(_.dynamics))

  def compiler(ctx: Context[StaticF]): ExprF[IR[ID]] => IR[ID] = {
    case x: InputF[_] => IR(ctx.rec(x), Bag.empty, Bag.empty)
    case x: CstF[_]   => IR(ctx.rec(x), Bag.empty, Bag.empty)
    case x @ DynamicF(params, instantiator, tpe) =>
      val id = ctx.rec(InputF(Ident(x.smap(_.value)), tpe))
      IR(
        value = id,
        provided = getProvided(x), //childrenFlatten(x.smap(_.provided)),
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
