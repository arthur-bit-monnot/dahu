package dahu.model.problem

import cats._
import dahu.recursion.{Fix, Recursion}
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

  def map[G[_]](f: F[ID] => G[ID]): IlazyForest[K, G, Opt] =
    new MappedLazyForest(f, this)

  def build(id: ID)(implicit F: SFunctor[F], ct: ClassTag[F[Fix[F]]]): Fix[F] = {
    Recursion.ana(i => internalCoalgebra(i))(id)
  }
}

class MappedLazyForest[K, F[_], G[_], Opt[_]](f: F[ID] => G[ID], mapped: IlazyForest[K, F, Opt])
    extends IlazyForest[K, G, Opt] {
  private val memo = mutable.HashMap[ID, G[ID]]()

  override def getTreeRoot(k: K): Opt[ID] = mapped.getTreeRoot(k)

  override def internalCoalgebra(i: ID): G[ID] =
    memo.getOrElseUpdate(i, f(mapped.internalCoalgebra(i)))
}

case class LazyTree[K, F[_], Opt[_]](root: ID, tree: IlazyForest[K, F, Opt]) {
  def map[G[_]](f: F[ID] => G[ID]): LazyTree[K, G, Opt] =
    LazyTree(root, tree.map(f))

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

final class Context[M[_]](val rec: M[ID] => ID, val retrieve: ID => M[ID])

/**
  *
  */
class LazyForestGenerator[K, F[_]: TreeNode: SFunctor, G[_], H[_]](
    coalgebra: K => F[K],
    algebraGenerator: Context[H] => F[G[ID]] => G[ID])(implicit ct: ClassTag[G[ID]])
    extends IlazyForest[K, H, G] {

  private val idsMap = mutable.HashMap[K, G[ID]]()
  private val repMap = mutable.ArrayBuffer[H[ID]]() // ID => H[ID]
  private val memo = mutable.HashMap[H[ID], ID]()

  private def record(e: H[ID]): ID = {
    if(memo.contains(e))
      memo(e)
    else {
      val id = repMap.size.asInstanceOf[ID]
      repMap += e
      memo += ((e, id))
      id
    }
  }
  private val ctx: Context[H] = new Context(record, internalCoalgebra)

  private val algebra: F[G[ID]] => G[ID] = algebraGenerator(ctx)

  @inline private def processed(k: K): Boolean = idsMap.contains(k)

  override def internalCoalgebra(i: ID): H[ID] = repMap(i)

  override def getTreeRoot(key: K): G[ID] = {
    val queue = mutable.Stack[K]()
    queue.push(key)

    while(queue.nonEmpty) {
      val cur = queue.pop()
      val fk = coalgebra(cur)
      if(the[TreeNode[F]].children(fk).forall(processed)) {
        val fg = the[SFunctor[F]].smap(fk)(idsMap)
        val g: G[ID] = algebra(fg)
        idsMap += ((cur, g))
      } else {
        queue.push(cur)
        queue.pushAll(the[TreeNode[F]].children(fk))
      }
    }
    idsMap(key)
  }
}
