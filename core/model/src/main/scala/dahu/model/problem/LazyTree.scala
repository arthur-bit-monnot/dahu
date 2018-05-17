package dahu.model.problem

import cats._
import cats.implicits._
import dahu.model.ir.{AST, ExprF}
import dahu.recursion.{EnvT, FAlgebra, FCoalgebra, Fix, Recursion}
import dahu.utils.{SFunctor, SubSubInt}
import shapeless.the

import scala.collection.mutable
import scala.reflect.ClassTag

trait OpaqueForest[K, F[_], Opt[_]] {
  type ID <: dahu.model.problem.IDTop

  sealed trait Marker
}

trait IlazyForest[K, F[_], Opt[_], InternalID <: IDTop] extends OpaqueForest[K, F, Opt] { self =>
  override type ID = InternalID
  def getExt(k: K)(implicit F: Functor[Opt]): Opt[F[ID]] =
    F.map(getTreeRoot(k))(internalCoalgebra)
  def getTreeRoot(k: K): Opt[ID]
  def internalCoalgebra(i: ID): F[ID]

  def fixID: IlazyForest[K, F, Opt, SubSubInt[IDTop, Marker]] =
    castIDTo[SubSubInt[IDTop, Marker]]
  private def castIDTo[NewInternalID <: IDTop]: IlazyForest[K, F, Opt, NewInternalID] =
    this.asInstanceOf[IlazyForest[K, F, Opt, NewInternalID]]

  def mapInternal[G[_]](f: F[ID] => G[ID]): IlazyForest[K, G, Opt, ID] =
    InternalMappedLazyForest(this)(f)

  def mapInternalGen[G[_]](
      f: InternalMapGenLazyForest.Context[F, G, ID, IDTop] => F[IDTop] => G[IDTop])(
      implicit TN: TreeNode[F],
      F: SFunctor[F],
      OF: Functor[Opt]
  ) = //: IlazyForest[K, G, Opt, _] =
    InternalMapGenLazyForest(this)(f)

  def mapExternal[Opt2[_]](f: Opt[ID] => Opt2[ID]): IlazyForest[K, F, Opt2, ID] =
    ExternalMappedLazyForest(this)(f)

  def build(id: ID)(implicit F: SFunctor[F], ct: ClassTag[F[Fix[F]]]): Fix[F] = {
    Recursion.ana(i => internalCoalgebra(i))(id)
  }

  def filter(f: F[ID] => Boolean)(
      implicit ev: Opt[InternalID] =:= InternalID): IlazyForest[K, F, Option, ID] =
    new IlazyForest[K, F, Option, InternalID] {
      override def getTreeRoot(k: K): Option[InternalID] = {
        val root = self.getTreeRoot(k)
        if(f(self.internalCoalgebra(root)))
          Some[ID](ev(root))
        else
          None
      }
      override def internalCoalgebra(i: InternalID): F[InternalID] = self.internalCoalgebra(i)
    }
}
object IlazyForest {
  def build[K, FIn[_]: TreeNode: SFunctor, FOut[_], Opt[_]](
      coalgebra: K => FIn[K],
      algebraGenerator: LazyForestGenerator.Context[FOut, IDTop] => FIn[Opt[IDTop]] => Opt[IDTop])(
      implicit ct: ClassTag[Opt[IDTop]]): IlazyForest[K, FOut, Opt, _] =
    new LazyForestGenerator[K, FIn, FOut, Opt, IDTop](coalgebra, algebraGenerator)
}

class InternalMappedLazyForest[K, F[_], G[_], Opt[_], InternalID <: IDTop] private (
    val mapped: IlazyForest[K, F, Opt, InternalID])(f: F[InternalID] => G[InternalID])
    extends IlazyForest[K, G, Opt, InternalID] {

  private val memo = mutable.HashMap[ID, G[ID]]()

  override def getTreeRoot(k: K): Opt[ID] = mapped.getTreeRoot(k)

  override def internalCoalgebra(i: ID): G[ID] =
    memo.getOrElseUpdate(i, f(mapped.internalCoalgebra(i)))
}
object InternalMappedLazyForest {
  def apply[K, F[_], G[_], Opt[_], I <: IDTop](mapped: IlazyForest[K, F, Opt, I])(
      f: F[I] => G[I]): IlazyForest[K, G, Opt, I] =
    new InternalMappedLazyForest[K, F, G, Opt, I](mapped)(f)
}

class ExternalMappedLazyForest[K, F[_], Opt[_], NewOpt[_], InternalID <: IDTop] private (
    val mapped: IlazyForest[K, F, Opt, InternalID])(f: Opt[InternalID] => NewOpt[InternalID])
    extends IlazyForest[K, F, NewOpt, InternalID] {

  private val memo = mutable.HashMap[K, NewOpt[ID]]()

  override def getTreeRoot(k: K): NewOpt[ID] = memo.getOrElseUpdate(k, f(mapped.getTreeRoot(k)))

  override def internalCoalgebra(i: ID): F[ID] =
    mapped.internalCoalgebra(i)
}
object ExternalMappedLazyForest {
  def apply[K, F[_], Opt[_], NewOpt[_], I <: IDTop](mapped: IlazyForest[K, F, Opt, I])(
      f: Opt[I] => NewOpt[I]): IlazyForest[K, F, NewOpt, I] =
    new ExternalMappedLazyForest[K, F, Opt, NewOpt, I](mapped)(f)
}

class LazyTree[K, F[_], Opt[_], InternalID <: IDTop] private (
    val tree: IlazyForest[K, F, Opt, InternalID])(_root: InternalID) {
  type ID = InternalID
  val root = _root.asInstanceOf[ID]

  def fixID: LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]] =
    this.asInstanceOf[LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]]]

  def map[G[_]](f: F[ID] => G[ID]): LazyTree[K, G, Opt, ID] =
    LazyTree(tree.mapInternal(f))(root)

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

  def apply[K, F[_], Opt[_], I <: IDTop](tree: IlazyForest[K, F, Opt, I])(
      root: tree.ID): LazyTree[K, F, Opt, tree.ID] =
    new LazyTree[K, F, Opt, tree.ID](tree)(root)

  def parse[K, F[_]: SFunctor: TreeNode](t: K,
                                         coalgebra: K => F[K]): IlazyForest[K, F, cats.Id, _] = {
    def algebra(ctx: LazyForestGenerator.Context[F, IDTop]): F[IDTop] => IDTop = ctx.record
    new LazyForestGenerator[K, F, F, cats.Id, IDTop](coalgebra, algebra)
  }
  def parse[K, FIn[_]: SFunctor: TreeNode, FOut[_]](
      t: K,
      coalgebra: K => FIn[K],
      algebra: LazyForestGenerator.Context[FOut, IDTop] => FIn[IDTop] => IDTop)
    : IlazyForest[K, FOut, cats.Id, _] = {
    new LazyForestGenerator[K, FIn, FOut, cats.Id, IDTop](coalgebra, algebra)
  }
}

class LazyForestGenerator[K, FIn[_]: TreeNode: SFunctor, FOut[_], Opt[_], InternalID <: IDTop](
    coalgebra: K => FIn[K],
    algebraGenerator: LazyForestGenerator.Context[FOut, InternalID] => FIn[Opt[InternalID]] => Opt[
      InternalID])(implicit ct: ClassTag[Opt[InternalID]])
    extends IlazyForest[K, FOut, Opt, InternalID] {

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
  private val ctx: LazyForestGenerator.Context[FOut, ID] =
    new LazyForestGenerator.Context(record, internalCoalgebra)

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

object LazyForestGenerator {

  final class Context[M[_], A <: Int](val rec: M[A] => A, val ret: A => M[A]) {
    def record(fa: M[A]): A = rec(fa)
    def retrieve(a: A): M[A] = ret(a)
  }
}

class InternalMapGenLazyForest[K, F[_]: TreeNode: SFunctor, G[_], Opt[_]: Functor, IDOrig <: IDTop] private (
    val orig: IlazyForest[K, F, Opt, IDOrig])(
    fFactory: InternalMapGenLazyForest.Context[F, G, IDOrig, IDTop] => F[IDTop] => G[IDTop])
    extends IlazyForest[K, G, Opt, IDTop] {

  private val treeNode = implicitly[TreeNode[F]]
  private val functor = implicitly[SFunctor[F]]

  private val idsMap = mutable.HashMap[orig.ID, ID]()
  private val repMap = mutable.ArrayBuffer[G[ID]]() // ID => G[ID]

  private[problem] def record(ga: G[ID]): ID = {
    val id = repMap.size.asInstanceOf[ID]
    repMap += ga
    id
  }
  private def retrieve(id: ID): G[ID] = repMap(id)

  private val f: F[ID] => G[ID] = fFactory(
    new InternalMapGenLazyForest.Context(record, retrieve, orig.internalCoalgebra))

  @inline private def processed(k: orig.ID): Boolean = {
    assert(!idsMap.contains(k) || repMap.size > idsMap(k))
    idsMap.contains(k)
  }

  def get(k: K): Opt[G[ID]] = {
    orig.getTreeRoot(k).map(getFromOrigID)
  }

  def getFromOrigID(origID: orig.ID): G[ID] = {
    val queue = mutable.Stack[orig.ID]()
    def push(origID: orig.ID): Unit = {
      if(!processed(origID)) {
        queue.push(origID)
      }
    }
    push(origID)
    while(queue.nonEmpty) {
      val cur = queue.pop()
      if(!processed(cur)) {
        val fk = orig.internalCoalgebra(cur)
        if(treeNode.children(fk).forall(processed)) {
          val fg: F[ID] = functor.smap(fk)(id => idsMap(id))
          val g: G[ID] = f(fg)
          val id = record(g)
          idsMap += ((cur, id))
        } else {
          push(cur)
          treeNode.children(fk).foreach(push)
        }
      }
    }
    repMap(idsMap(origID))
  }

  override def internalCoalgebra(i: ID): G[ID] = repMap(i)

  override def getTreeRoot(k: K): Opt[ID] = getExt(k).map(record)
}

object InternalMapGenLazyForest {
  def apply[K, F[_]: TreeNode: SFunctor, G[_], Opt[_]: Functor, IDOrig <: IDTop](
      orig: IlazyForest[K, F, Opt, IDOrig])(
      fFactory: Context[F, G, IDOrig, IDTop] => F[IDTop] => G[IDTop])
    : InternalMapGenLazyForest[K, F, G, Opt, IDOrig] =
    new InternalMapGenLazyForest[K, F, G, Opt, IDOrig](orig)(fFactory)

  final class Context[F[_], G[_], IDOld <: Int, ID <: Int](rec: G[ID] => ID,
                                                           ret: ID => G[ID],
                                                           retOld: IDOld => F[IDOld]) {
    def record(fa: G[ID]): ID = rec(fa)
    def retrieve(a: ID): G[ID] = ret(a)
    def retrieveOld(b: IDOld): F[IDOld] = retOld(b)
  }
}
