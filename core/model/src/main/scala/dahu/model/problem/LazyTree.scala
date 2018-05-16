package dahu.model.problem

import cats._
import dahu.model.ir.{AST, ExprF}
import dahu.recursion.{EnvT, FAlgebra, FCoalgebra, Fix, Recursion}
import dahu.utils.SFunctor
import shapeless.the

import scala.collection.mutable
import scala.reflect.ClassTag

trait IlazyForest[K, F[_], Opt[_]] {
  type ID = dahu.model.problem.ID
  def getExt(k: K)(implicit F: Functor[Opt]): Opt[F[ID]] =
    F.map(getTreeRoot(k))(internalCoalgebra)
  def getTreeRoot(k: K): Opt[ID]
  def internalCoalgebra(i: ID): F[ID]

  def mapInternal[G[_]](f: F[ID] => G[ID]): IlazyForest[K, G, Opt] =
    new InternalMappedLazyForest(f, this)

  def mapInternalGenerative[G[_]](f: Context[G] => F[ID] => G[ID]): IlazyForest[K, G, Opt] = ???

  def mapExternal[Opt2[_]](f: Opt[ID] => Opt2[ID]): IlazyForest[K, F, Opt2] =
    new ExternalMappedLazyForest(f, this)

  def build(id: ID)(implicit F: SFunctor[F], ct: ClassTag[F[Fix[F]]]): Fix[F] = {
    Recursion.ana(i => internalCoalgebra(i))(id)
  }
}

class InternalMappedLazyForest[K, F[_], G[_], Opt[_]](f: F[ID] => G[ID],
                                                      mapped: IlazyForest[K, F, Opt])
    extends IlazyForest[K, G, Opt] {
  private val memo = mutable.HashMap[ID, G[ID]]()

  override def getTreeRoot(k: K): Opt[ID] = mapped.getTreeRoot(k)

  override def internalCoalgebra(i: ID): G[ID] =
    memo.getOrElseUpdate(i, f(mapped.internalCoalgebra(i)))
}

class ExternalMappedLazyForest[K, F[_], Opt[_], NewOpt[_]](f: Opt[ID] => NewOpt[ID],
                                                           mapped: IlazyForest[K, F, Opt])
    extends IlazyForest[K, F, NewOpt] {
  private val memo = mutable.HashMap[K, NewOpt[ID]]()

  override def getTreeRoot(k: K): NewOpt[ID] = memo.getOrElseUpdate(k, f(mapped.getTreeRoot(k)))

  override def internalCoalgebra(i: ID): F[ID] =
    mapped.internalCoalgebra(i)
}

case class LazyTree[K, F[_], Opt[_]](root: ID, tree: IlazyForest[K, F, Opt]) {
  def map[G[_]](f: F[ID] => G[ID]): LazyTree[K, G, Opt] =
    LazyTree(root, tree.mapInternal(f))

  def nodes(implicit tn: TreeNode[F]): Seq[(tree.ID, F[tree.ID])] = {
    val queue = mutable.Stack[tree.ID]()
    val visited = mutable.HashSet[tree.ID]()
    val result = mutable.ArrayBuffer[(tree.ID, F[tree.ID])]()
    queue.push(root)

    while(queue.nonEmpty) {
      val cur = queue.pop()
      val fcur = tree.internalCoalgebra(cur)
      if(tn.children(fcur).forall(visited)) {
        visited += cur
        result += ((cur, fcur))
      } else {
        queue.push(cur)
        queue.pushAll(tn.children(fcur))
      }
    }
    result.toList
  }

  def fullTree(implicit F: SFunctor[F], ct: ClassTag[F[Fix[F]]]): Fix[F] = tree.build(root)
}

object LazyTree {

  def parse[K, F[_]: SFunctor: TreeNode](t: K, coalgebra: K => F[K]): IlazyForest[K, F, cats.Id] = {
    def algebra(ctx: Context[F]): F[ID] => ID = ctx.rec
    new LazyForestGenerator[K, F, F, cats.Id](coalgebra, algebra)
  }
  def parse[K, FIn[_]: SFunctor: TreeNode, FOut[_]](
      t: K,
      coalgebra: K => FIn[K],
      algebra: Context[FOut] => FIn[ID] => ID): IlazyForest[K, FOut, cats.Id] = {
    new LazyForestGenerator[K, FIn, FOut, cats.Id](coalgebra, algebra)
  }
}

final class Context[M[_]](val rec: M[ID] => ID, val retrieve: ID => M[ID])

class LazyForestGenerator[K, FIn[_]: TreeNode: SFunctor, FOut[_], Opt[_]](
    coalgebra: K => FIn[K],
    algebraGenerator: Context[FOut] => FIn[Opt[ID]] => Opt[ID])(implicit ct: ClassTag[Opt[ID]])
    extends IlazyForest[K, FOut, Opt] {

  private val idsMap = mutable.HashMap[K, Opt[ID]]()
  private val repMap = mutable.ArrayBuffer[FOut[ID]]() // ID => H[ID]
  private val memo = mutable.HashMap[FOut[ID], ID]()

  private def record(e: FOut[ID]): ID = {
    if(memo.contains(e))
      memo(e)
    else {
      val id = repMap.size.asInstanceOf[ID]
      repMap += e
      memo += ((e, id))
      id
    }
  }
  private val ctx: Context[FOut] = new Context(record, internalCoalgebra)

  private val algebra: FIn[Opt[ID]] => Opt[ID] = algebraGenerator(ctx)

  @inline private def processed(k: K): Boolean = idsMap.contains(k)

  override def internalCoalgebra(i: ID): FOut[ID] = repMap(i)

  override def getTreeRoot(key: K): Opt[ID] = {
    val queue = mutable.Stack[K]()
    queue.push(key)

    while(queue.nonEmpty) {
      val cur = queue.pop()
      val fk = coalgebra(cur)
      if(the[TreeNode[FIn]].children(fk).forall(processed)) {
        val fg = the[SFunctor[FIn]].smap(fk)(idsMap)
        val g: Opt[ID] = algebra(fg)
        idsMap += ((cur, g))
      } else {
        queue.push(cur)
        queue.pushAll(the[TreeNode[FIn]].children(fk))
      }
    }
    idsMap(key)
  }
}
