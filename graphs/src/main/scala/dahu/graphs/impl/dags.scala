package dahu.graphs.impl

import dahu.graphs._

import cats._
import cats.implicits._
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.recursion.{Fix, Recursion}
import dahu.utils.{BiMap, Graph, SFunctor, SubSubInt}
import dahu.utils._

import scala.collection.mutable
import scala.reflect.ClassTag

trait LazyForestLayer[K, F[_], Opt[_], OwnID <: IDTop, PrevID <: IDTop]
    extends OpenASG[K, F, Opt, OwnID] {
  override type ID = OwnID
  def fromPreviousId(id: PrevID): ID
  override def fixID: LazyForestLayer[K, F, Opt, SubSubInt[IDTop, Marker], PrevID] =
    this.asInstanceOf[LazyForestLayer[K, F, Opt, SubSubInt[IDTop, Marker], PrevID]]

  def record(fi: F[ID]): ID
}

class InternalMappedLazyForest[K, F[_], G[_], Opt[_], InternalID <: IDTop] private (
    val mapped: OpenASG[K, F, Opt, InternalID])(f: F[InternalID] => G[InternalID])
    extends OpenASG[K, G, Opt, InternalID] {

  private val memo = mutable.HashMap[ID, G[ID]]()

  override def getTreeRoot(k: K): Opt[ID] = mapped.getTreeRoot(k)

  override def internalCoalgebra(i: ID): G[ID] =
    memo.getOrElseUpdate(i, f(mapped.internalCoalgebra(i)))
}
object InternalMappedLazyForest {
  def apply[K, F[_], G[_], Opt[_], I <: IDTop](mapped: OpenASG[K, F, Opt, I])(
      f: F[I] => G[I]): OpenASG[K, G, Opt, I] =
    new InternalMappedLazyForest[K, F, G, Opt, I](mapped)(f)
}

class ExternalMappedLazyForest[K, F[_], Opt[_], NewOpt[_], InternalID <: IDTop] private (
    val mapped: OpenASG[K, F, Opt, InternalID])(f: Opt[InternalID] => NewOpt[InternalID])
    extends OpenASG[K, F, NewOpt, InternalID] {

  private val memo = mutable.HashMap[K, NewOpt[ID]]()

  override def getTreeRoot(k: K): NewOpt[ID] = memo.getOrElseUpdate(k, f(mapped.getTreeRoot(k)))

  override def internalCoalgebra(i: ID): F[ID] =
    mapped.internalCoalgebra(i)
}
object ExternalMappedLazyForest {
  def apply[K, F[_], Opt[_], NewOpt[_], I <: IDTop](mapped: OpenASG[K, F, Opt, I])(
      f: Opt[I] => NewOpt[I]): OpenASG[K, F, NewOpt, I] =
    new ExternalMappedLazyForest[K, F, Opt, NewOpt, I](mapped)(f)
}

class LazyForestGenerator[K, FIn[_]: TreeNode: SFunctor, FOut[_], Opt[_], InternalID <: IDTop](
    coalgebra: K => FIn[K],
    algebraGenerator: LazyForestGenerator.Context[FOut, InternalID] => FIn[Opt[InternalID]] => Opt[
      InternalID])(implicit ct: ClassTag[Opt[InternalID]])
    extends OpenASG[K, FOut, Opt, InternalID] {

  val idsMap = mutable.HashMap[K, Opt[ID]]()
  private val repMap = mutable.ArrayBuffer[FOut[ID]]() // ID => H[ID]
  private val memo = mutable.HashMap[FOut[ID], ID]()

  // TODO make private once IntBoolSatiscationProblem is fixed
  def record(e: FOut[ID]): ID = {
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
    if(processed(key))
      return idsMap(key)

    val queue = mutable.Stack[K]()
    queue.push(key)

    while(queue.nonEmpty) {
      val cur = queue.pop()
      if(!processed(cur)) {
        val fk = coalgebra(cur)
        if(fk.forallChildren(processed)) {
          val fg = implicitly[SFunctor[FIn]].smap(fk)(idsMap)
          val g: Opt[ID] = algebra(fg)
          idsMap += ((cur, g))
        } else {
          queue.push(cur)
          fk.foreachChild(queue.push(_))
        }
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
    val orig: OpenASG[K, F, Opt, IDOrig])(
    fFactory: InternalMapGenLazyForest.Context[F, G, IDOrig, IDTop] => F[IDTop] => G[IDTop])
    extends OpenASG[K, G, Opt, IDTop] {

  private val functor = implicitly[SFunctor[F]]

  private val idsMap = BiMap[orig.ID, ID]()
  private val repMap = mutable.HashMap[ID, G[ID]]()
  private var nextID = 0
  private def getNewID(): ID = { nextID += 1; (nextID - 1).asInstanceOf[ID] }

  private[dahu] def record(ga: G[ID]): ID = {
    val id = getNewID()
    repMap += ((id, ga))
    id
  }

  private lazy val f: F[ID] => G[ID] = fFactory(
    new InternalMapGenLazyForest.Context(record,
                                         internalCoalgebra,
                                         orig.internalCoalgebra,
                                         id => oldToNewId(id)))

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
      orig: OpenASG[K, F, Opt, IDOrig])(
      fFactory: Context[F, G, IDOrig, IDTop] => F[IDTop] => G[IDTop])
    : InternalMapGenLazyForest[K, F, G, Opt, IDOrig] =
    new InternalMapGenLazyForest[K, F, G, Opt, IDOrig](orig)(fFactory)

  final class Context[F[_], G[_], IDOld <: Int, ID <: Int](val record: G[ID] => ID,
                                                           val retrieve: ID => G[ID],
                                                           val retrieveOld: IDOld => F[IDOld],
                                                           val toNewId: IDOld => ID)
}

//TODO: make constructor private
class FullMapGenLazyForest[K, F[_]: SFunctor, G[_], Opt[_]: Functor, Opt2[_], IDOrig <: IDTop](
    val orig: OpenASG[K, F, Opt, IDOrig])(
    fFactory: FullMapGenLazyForest.Generator[F, G, Opt, Opt2, IDOrig])
    extends OpenASG[K, G, Opt2, IDTop] {

  private val functor = implicitly[SFunctor[F]]

  private val idsMap = BiMap[orig.ID, ID]()
  private val repMap = mutable.HashMap[ID, G[ID]]()
  private val optMap = mutable.Map[Opt[IDOrig], Opt2[ID]]()
  private var nextID = 0
  private def getNewID(): ID = { nextID += 1; (nextID - 1).asInstanceOf[ID] }

  private[dahu] def record(ga: G[ID]): ID = {
    val id = getNewID()
    repMap += ((id, ga))
    id
  }
  private val ctx = new InternalMapGenLazyForest.Context(record,
                                                         internalCoalgebra,
                                                         orig.internalCoalgebra,
                                                         id => oldToNewId(id))
  private val fi: F[ID] => G[ID] = fFactory.internalMap(ctx)
  private val fe: Opt[IDOrig] => Opt2[ID] = fFactory.externalMap(ctx)

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
      val ga = fi(fa)
      repMap += ((i, ga))
    }
    repMap(i)
  }

  override def getTreeRoot(k: K): Opt2[ID] = {
    val fi = orig.getTreeRoot(k)
    optMap.getOrElseUpdate(fi, fe(fi))
  }
}

object FullMapGenLazyForest {

  trait Generator[F[_], G[_], Opt[_], Opt2[_], I <: IDTop] {

    def internalMap(ctx: InternalMapGenLazyForest.Context[F, G, I, IDTop])(fi: F[IDTop]): G[IDTop]
    def externalMap(ctx: InternalMapGenLazyForest.Context[F, G, I, IDTop])(oi: Opt[I]): Opt2[IDTop]
  }
}
