package dahu.model.problem

import cats._
import cats.implicits._
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.model.ir.{AST, ExprF}
import dahu.recursion.{EnvT, FAlgebra, FCoalgebra, Fix, Recursion}
import dahu.utils.{BiMap, SFunctor, SubSubInt}
import dahu.utils.SFunctor._
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
      implicit F: SFunctor[F],
      OF: Functor[Opt]
  ) = //: IlazyForest[K, G, Opt, _] =
    InternalMapGenLazyForest(this)(f)

  def mapContextualized[G[_]](
      f: LazyForestGenerator.Context[G, IDTop] => F[IDTop] => IDTop,
      contextTracker: ContextualLazyForestMap.ContextualPreprocessor[F, ID])(
      implicit TN: TreeNode[F],
      F: SFunctor[F],
      FO: Functor[Opt]) = //: IlazyForest[K, G, Opt, _] = TODO: this is need because LazyTree use an internal id
    new ContextualLazyForestMap[K, F, G, Opt, IDTop, ID](this, f, contextTracker)

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

  def nodes(root: ID)(implicit tn: TreeNode[F]): Seq[(ID, F[ID])] = {
    val queue = mutable.Stack[ID]()
    val visited = mutable.HashSet[ID]()
    val result = mutable.ArrayBuffer[(ID, F[ID])]()
    queue.push(root)

    while(queue.nonEmpty) {
      val cur = queue.pop()
      val fcur = internalCoalgebra(cur)
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

  def changedKey[K2](f: K2 => K): IlazyForest[K2, F, Opt, self.ID] =
    new IlazyForest[K2, F, Opt, self.ID] {
      override def getTreeRoot(k: K2): Opt[self.ID] = self.getTreeRoot(f(k))

      override def internalCoalgebra(i: self.ID): F[self.ID] = self.internalCoalgebra(i)
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
    val tree: IlazyForest[K, F, Opt, InternalID],
    val root: K) {
  type ID = InternalID

  def fixID: LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]] =
    this.asInstanceOf[LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]]]

  def map[G[_]](f: F[ID] => G[ID]): LazyTree[K, G, Opt, ID] =
    LazyTree(tree.mapInternal(f))(root)

  def mapExternal[Opt2[_]](f: Opt[ID] => Opt2[ID]): LazyTree[K, F, Opt2, ID] =
    LazyTree(tree.mapExternal(f))(root)

  def fullTree(implicit F: SFunctor[F], FO: Functor[Opt], ct: ClassTag[F[Fix[F]]]): Opt[Fix[F]] =
    tree.getTreeRoot(root).map(tree.build)

  def nodes(implicit tn: TreeNode[F], F: Functor[Opt]): Opt[Seq[(tree.ID, F[tree.ID])]] =
    tree.getTreeRoot(root).map(tree.nodes)
}

object LazyTree {

  def apply[K, F[_], Opt[_], I <: IDTop](tree: IlazyForest[K, F, Opt, I])(
      root: K): LazyTree[K, F, Opt, tree.ID] =
    new LazyTree[K, F, Opt, tree.ID](tree, root)

  def parse[K, F[_]: SFunctor: TreeNode](t: K, coalgebra: K => F[K]): LazyTree[K, F, cats.Id, _] = {
    def algebra(ctx: LazyForestGenerator.Context[F, IDTop]): F[IDTop] => IDTop = ctx.record
    val forest = new LazyForestGenerator[K, F, F, cats.Id, IDTop](coalgebra, algebra)
    LazyTree(forest)(t)
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

class InternalMapGenLazyForest[K, F[_]: SFunctor, G[_], Opt[_]: Functor, IDOrig <: IDTop] private (
    val orig: IlazyForest[K, F, Opt, IDOrig])(
    fFactory: InternalMapGenLazyForest.Context[F, G, IDOrig, IDTop] => F[IDTop] => G[IDTop])
    extends IlazyForest[K, G, Opt, IDTop] {

  private val functor = implicitly[SFunctor[F]]

  private val idsMap = BiMap[orig.ID, ID]()
  private val repMap = mutable.HashMap[ID, G[ID]]()
  private var nextID = 0
  private def getNewID(): ID = { nextID += 1; (nextID - 1).asInstanceOf[ID] }

  private[problem] def record(ga: G[ID]): ID = {
    val id = getNewID()
    repMap += ((id, ga))
    id
  }

  private lazy val f: F[ID] => G[ID] = fFactory(
    new InternalMapGenLazyForest.Context(record,
                                         internalCoalgebra,
                                         orig.internalCoalgebra,
                                         id => oldToNewId(id)))

  @inline private def processed(k: orig.ID): Boolean = {
    idsMap.contains(k) && repMap.contains(idsMap.get(k))
  }

  def get(k: K): Opt[G[ID]] = {
    orig.getTreeRoot(k).map(getFromOrigID)
  }

  def oldToNewId(origID: orig.ID): ID = {
    if(!idsMap.contains(origID)) {
      val id = getNewID()
      idsMap.add(origID, id)
    }
    idsMap.get(origID)
  }

  def getFromOrigID(origID: orig.ID): G[ID] = {
    internalCoalgebra(oldToNewId(origID))
  }

  override def internalCoalgebra(i: ID): G[ID] = {
    if(!repMap.contains(i)) {
      assert(idsMap.cocontains(i))
      val origID = idsMap.coget(i)
      val fk = orig.internalCoalgebra(origID)
      val fa = functor.smap(fk)(oldToNewId)
      val ga = f(fa)
      repMap += ((i, ga))
    }
    repMap(i)
  }

  override def getTreeRoot(k: K): Opt[ID] = orig.getTreeRoot(k).map(oldToNewId)
}

object InternalMapGenLazyForest {

  def apply[K, F[_]: SFunctor, G[_], Opt[_]: Functor, IDOrig <: IDTop](
      orig: IlazyForest[K, F, Opt, IDOrig])(
      fFactory: Context[F, G, IDOrig, IDTop] => F[IDTop] => G[IDTop])
    : InternalMapGenLazyForest[K, F, G, Opt, IDOrig] =
    new InternalMapGenLazyForest[K, F, G, Opt, IDOrig](orig)(fFactory)

  final class Context[F[_], G[_], IDOld <: Int, ID <: Int](val record: G[ID] => ID,
                                                           val retrieve: ID => G[ID],
                                                           val retrieveOld: IDOld => F[IDOld],
                                                           val toNewId: IDOld => ID)
}

class ContextualLazyForestMap[
    K,
    FIn[_]: TreeNode: SFunctor,
    FOut[_],
    Opt[_]: Functor,
    InternalID <: IDTop,
    OID <: IDTop](base: IlazyForest[K, FIn, Opt, OID],
                  compilerGenerator: LazyForestGenerator.Context[FOut, InternalID] => FIn[
                    InternalID] => InternalID,
                  contextualPreprocessor: ContextualLazyForestMap.ContextualPreprocessor[FIn, OID])
    extends IlazyForest[K, FOut, Opt, InternalID] {

  private type PrePro = ContextualLazyForestMap.ContextualPreprocessor[FIn, OID]

  private val intIdsMap = mutable.HashMap[(OID, PrePro), ID]()
  private val repMap = mutable.ArrayBuffer[FOut[ID]]() // ID => FOut[ID]
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
  private val generationContext: LazyForestGenerator.Context[FOut, ID] =
    new LazyForestGenerator.Context(record, internalCoalgebra)

  def algebra: FIn[ID] => ID = compilerGenerator(generationContext)

  override def internalCoalgebra(i: ID): FOut[ID] = repMap(i)

  def getFromOriginalId(oid: base.ID): ID = {
    val queue = mutable.Stack[(OID, PrePro)]()
    queue.push((oid, contextualPreprocessor))

    while(queue.nonEmpty) {
      val (cur, ctx) = queue.pop()
      val fkNoPrepro = base.internalCoalgebra(cur) //originalCoalgebra(cur)
      val subCtx = ctx.subPreprocessor(base.internalCoalgebra, fkNoPrepro)
      val fk = fkNoPrepro.smap(subCtx.prepro)
      if(fk.children.forall(c => intIdsMap.contains((c, subCtx)))) {
        val fg = fk.smap(c => intIdsMap((c, subCtx)))
        val g: ID = algebra(fg)
        intIdsMap += (((cur, ctx), g))
      } else {
        queue.push((cur, ctx))

        fk.children.foreach(c => queue.push((c, subCtx)))
      }
    }
    intIdsMap((oid, contextualPreprocessor))
  }

  override def getTreeRoot(key: K): Opt[ID] = {
    base.getTreeRoot(key).map(getFromOriginalId)
  }
}

object ContextualLazyForestMap {
  abstract class ContextualPreprocessor[F[_], I <: Int] {
    def subPreprocessor(coalg: I => F[I], fi: F[I]): ContextualPreprocessor[F, I]
    def prepro(i: I): I
  }
}
