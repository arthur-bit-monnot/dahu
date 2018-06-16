package dahu.model.problem

import cats._
import cats.implicits._
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.model.ir.ComputationF
import dahu.model.math.bool
import dahu.recursion.{Fix, Recursion}
import dahu.utils.{BiMap, Graph, SFunctor, SubSubInt}
import dahu.utils._
import shapeless.the

import scala.collection.mutable
import scala.reflect.ClassTag

trait OpaqueForest[K, F[_], Opt[_]] {
  type ID <: dahu.model.problem.IDTop

  sealed trait Marker
}

trait LazyMap[K, V, Opt[_], I] {
  def get(k: K)(implicit F: Functor[Opt]): Opt[V]
  def getInternal(i: I): V

  def asFunction(implicit F: Functor[Opt]): K => Opt[V] = get
  def asInternalFunction: I => V = getInternal
}

trait IlazyForest[K, F[_], Opt[_], InternalID <: IDTop] extends OpaqueForest[K, F, Opt] { self =>
  override type ID = InternalID
  def getExt(k: K)(implicit F: Functor[Opt]): Opt[F[ID]] =
    F.map(getTreeRoot(k))(internalCoalgebra)

  def getTreeRoot(k: K): Opt[ID]

  def internalCoalgebra(i: ID): F[ID]

  def fixID: IlazyForest[K, F, Opt, SubSubInt[IDTop, Marker]] =
    castIDTo[SubSubInt[IDTop, Marker]]
  def castIDTo[NewInternalID <: IDTop]: IlazyForest[K, F, Opt, NewInternalID] =
    this.asInstanceOf[IlazyForest[K, F, Opt, NewInternalID]]

  def forceEvaluation(k: K): Unit = {
    getTreeRoot(k)
  }

  def mapInternal[G[_]](f: F[ID] => G[ID]): IlazyForest[K, G, Opt, ID] =
    InternalMappedLazyForest(this)(f)

  def mapInternalGen[G[_]](
      f: InternalMapGenLazyForest.Context[F, G, ID, IDTop] => F[IDTop] => G[IDTop])(
      implicit F: SFunctor[F],
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

      override def toString: String = "ILazyForestChangedKey"
    }

  def cata[V: ClassTag](f: F[V] => V)(implicit T: TreeNode[F],
                                      F: SFunctor[F]): LazyMap[K, V, Opt, ID] =
    new LazyMap[K, V, Opt, ID] {
      private val values = debox.Map[ID, V]()

      private def hasValue(i: ID): Boolean = values.contains(i)
      private def getValue(i: ID): V = values(i)
      private def setValue(i: ID, v: V): Unit = values.update(i, v)

      def getInternal(i: ID): V = {
        if(!hasValue(i)) {
          val queue = debox.Buffer[ID]()
          @inline def push(i: ID): Unit = queue += i
          @inline def pop(): ID = { queue.remove(queue.length - 1) }
          push(i)
          while(!queue.isEmpty) {
            val cur = pop()
            if(!hasValue(cur)) {
              val fcur = self.internalCoalgebra(cur)
              val children = TreeNode[F].children(fcur)
              if(children.forall(hasValue)) {
                val fcurv = SFunctor[F].smap(fcur)(getValue)
                setValue(cur, f(fcurv))
              } else {
                push(cur)
                children.foreach(push)
              }
            }
          }
        }
        getValue(i)
      }

      override def get(k: K)(implicit F: Functor[Opt]): Opt[V] =
        self.getTreeRoot(k).map(getInternal)
    }

  def cataLow[V: ClassTag](f: (ID, F[V]) => V)(implicit T: TreeNode[F],
                                               F: SFunctor[F]): LazyMap[K, V, Opt, ID] =
    new LazyMap[K, V, Opt, ID] {
      private val values = debox.Map[ID, V]()

      private def hasValue(i: ID): Boolean = values.contains(i)
      private def getValue(i: ID): V = values(i)
      private def setValue(i: ID, v: V): Unit = values.update(i, v)

      def getInternal(i: ID): V = {
        if(!hasValue(i)) {
          val queue = debox.Buffer[ID]()
          @inline def push(i: ID): Unit = queue += i
          @inline def pop(): ID = { queue.remove(queue.length - 1) }
          push(i)
          while(!queue.isEmpty) {
            val cur = pop()
            if(!hasValue(cur)) {
              val fcur = self.internalCoalgebra(cur)
              val children = TreeNode[F].children(fcur)
              if(children.forall(hasValue)) {
                val fcurv = SFunctor[F].smap(fcur)(getValue)
                setValue(cur, f(cur, fcurv))
              } else {
                push(cur)
                children.foreach(push)
              }
            }
          }
        }
        getValue(i)
      }

      override def get(k: K)(implicit F: Functor[Opt]): Opt[V] =
        self.getTreeRoot(k).map(getInternal)
    }

  def cataLow2[V: ClassTag](f: F[(V, ID)] => V)(implicit T: TreeNode[F],
                                                F: SFunctor[F]): LazyMap[K, V, Opt, ID] =
    new LazyMap[K, V, Opt, ID] {
      private val values = debox.Map[ID, V]()

      private def hasValue(i: ID): Boolean = values.contains(i)
      private def getValue(i: ID): V = values(i)
      private def setValue(i: ID, v: V): Unit = values.update(i, v)

      def getInternal(i: ID): V = {
        if(!hasValue(i)) {
          val queue = debox.Buffer[ID]()
          @inline def push(i: ID): Unit = queue += i
          @inline def pop(): ID = { queue.remove(queue.length - 1) }
          push(i)
          while(!queue.isEmpty) {
            val cur = pop()
            if(!hasValue(cur)) {
              val fcur = self.internalCoalgebra(cur)
              val children = TreeNode[F].children(fcur)
              if(children.forall(hasValue)) {
                val fcurv = SFunctor[F].smap(fcur)(x => (getValue(x), x))
                setValue(cur, f(fcurv))
              } else {
                push(cur)
                children.foreach(push)
              }
            }
          }
        }
        getValue(i)
      }

      override def get(k: K)(implicit F: Functor[Opt]): Opt[V] =
        self.getTreeRoot(k).map(getInternal)
    }

  def transform[I <: IDTop](fGen: (I => F[I], F[I] => I) => (F[I] => F[I]))(
      implicit TN: TreeNode[F],
      F: Functor[Opt],
      SF: SFunctor[F]): LazyForestLayer[K, F, Opt, _, ID] =
    new LazyForestLayer[K, F, Opt, I, self.ID] {
      val idMap = debox.Map[self.ID, I]()
      val coalg = BiMap[I, F[I]]()
      private var _nextID = 0
      def nextID(): I = { _nextID += 1; (_nextID - 1).asInstanceOf[I] }
      def directRecord(fi: F[I]): I = {
        if(!coalg.cocontains(fi)) {
          println(s"REC: $fi")
          if(fi.toString == "and(5, 7)")
            println("OOO")
          coalg.add(nextID(), fi)
        }
        coalg.coget(fi)
      }
      private val knownTransformations = mutable.HashMap[F[I], I]()
      private val pendingTransformations = mutable.Set[F[I]]()
      override def record(fi: F[I]): I = {
        if(coalg.cocontains(fi))
          coalg.coget(fi) // a normally already transformed value is already recorded, use its ID
        else if(knownTransformations.contains(fi))
          knownTransformations(fi)
        else if(pendingTransformations.contains(fi))
//          throw RecursiveTransformation
          directRecord(fi) // transform was recursively invoked
        else {
//          if(fi.toString() == "not(0)")
//            println("X")
//          println("1 " + fi + "  " + pendingTransformations + " ---- " + coalg.iterator.toList)
          pendingTransformations += fi
//          println("2 " + fi + "  " + pendingTransformations + " ---- " + coalg.iterator.toList)
          val fi2 = transform(fi)
//          println("3 " + fi + "  " + pendingTransformations + " ---- " + coalg.iterator.toList)
          pendingTransformations -= fi
//          println("4 " + fi + "  " + pendingTransformations + " ---- " + coalg.iterator.toList)
//          if(fi2.isInstanceOf[ComputationF[I]])
//            fi2.asInstanceOf[ComputationF[I]] match {
//              case ComputationF(bool.And, Vec(a), _) =>
//                println("STRANGE")
//              case _ =>
//            }
          val i = directRecord(fi2)
          knownTransformations.update(fi, i)
          i
        }
      }
      val transform: F[I] => F[I] = fGen(coalg.get(_), record)
      def processed(id: self.ID): Boolean = idMap.contains(id)
      override def fromPreviousId(id: self.ID): I = {
        if(!idMap.contains(id)) {
          // note: this is suboptimal as we might compute the topo order can contain nodes that are discarded with the filter on to process
          val toProcess = self.internalBottomUpTopologicalOrder(id)
          toProcess.withFilter(!processed(_)).foreach { cur =>
            assert(!idMap.contains(cur))
            val fcur: F[self.ID] = self.internalCoalgebra(cur)
            val fx: F[I] = fcur.smap(i => idMap(i))
            val fy = transform(fx)
            val y = directRecord(fy)
            idMap.update(cur, y)
          }
        }
        idMap(id)
      }

      override def getTreeRoot(k: K): Opt[I] = self.getTreeRoot(k).map(fromPreviousId(_))

      override def internalCoalgebra(i: I): F[I] = coalg.get(i)
    }

  def internalBottomUpTopologicalOrder(id: ID)(implicit TN: TreeNode[F]): Iterable[ID] = {
    Graph.topologicalOrderLeavesToRoot[ID, F](id, internalCoalgebra(_), TN.children(_))
  }
}

case object RecursiveTransformation extends Exception

object IlazyForest {
  def build[K, FIn[_]: TreeNode: SFunctor, FOut[_], Opt[_]](coalgebra: K => FIn[K])(
      algebraGenerator: LazyForestGenerator.Context[FOut, IDTop] => FIn[Opt[IDTop]] => Opt[IDTop])(
      implicit ct: ClassTag[Opt[IDTop]]): IlazyForest[K, FOut, Opt, _] =
    new LazyForestGenerator[K, FIn, FOut, Opt, IDTop](coalgebra, algebraGenerator)

  def ana[K, F[_]: SFunctor: TreeNode](coalgebra: K => F[K]) = { //: IlazyForest[K, F, cats.Id, _] = {
    def algebra(ctx: LazyForestGenerator.Context[F, IDTop]): F[IDTop] => IDTop = ctx.record
    new LazyForestGenerator[K, F, F, cats.Id, IDTop](coalgebra, algebra)
  }

  def anaGen[K, FIn[_]: SFunctor: TreeNode, FOut[_]](
      t: K,
      coalgebra: K => FIn[K],
      algebra: LazyForestGenerator.Context[FOut, IDTop] => FIn[IDTop] => IDTop)
    : IlazyForest[K, FOut, cats.Id, _] =
    new LazyForestGenerator[K, FIn, FOut, cats.Id, IDTop](coalgebra, algebra)

}
trait LazyForestLayer[K, F[_], Opt[_], OwnID <: IDTop, PrevID <: IDTop]
    extends IlazyForest[K, F, Opt, OwnID] {
  override type ID = OwnID
  def fromPreviousId(id: PrevID): ID
  override def fixID: LazyForestLayer[K, F, Opt, SubSubInt[IDTop, Marker], PrevID] =
    this.asInstanceOf[LazyForestLayer[K, F, Opt, SubSubInt[IDTop, Marker], PrevID]]

  def record(fi: F[ID]): ID
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
  final type ID = InternalID

  def fixID: LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]] =
    this.asInstanceOf[LazyTree[K, F, Opt, SubSubInt[IDTop, tree.Marker]]]

  def forceEvaluation: LazyTree[K, F, Opt, InternalID] = { tree.forceEvaluation(root); this }

//  def postpro[I <: IDTop](fGen: (I => F[I], F[I] => I) => (F[I] => F[I]))(
//      implicit TN: TreeNode[F],
//      F: Functor[Opt],
//      SF: SFunctor[F]): LazyTree[K, F, Opt, InternalID] = {
//    val t = tree.transform(fGen)
//    LazyTree(t)(root)
//  }

  def mapK[G[_]](fk: F ~> G): LazyTree[K, G, Opt, ID] = map(a => fk(a))
  def map[G[_]](f: F[ID] => G[ID]): LazyTree[K, G, Opt, ID] =
    LazyTree(tree.mapInternal(f))(root)

  def eval[V: ClassTag](
      f: F[V] => V)(implicit F: Functor[Opt], SF: SFunctor[F], T: TreeNode[F]): Opt[V] =
    tree.cata(f).get(root)

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
  def parseGen[K, FIn[_]: SFunctor: TreeNode, FOut[_]](
      t: K,
      coalgebra: K => FIn[K],
      algebra: LazyForestGenerator.Context[FOut, IDTop] => FIn[IDTop] => IDTop) =
    //:  IlazyForest[K, FOut, cats.Id, _] = TODO: make opaque once IntBoolSatisfactionProblem is clean
    new LazyForestGenerator[K, FIn, FOut, cats.Id, IDTop](coalgebra, algebra)

}

class LazyForestGenerator[K, FIn[_]: TreeNode: SFunctor, FOut[_], Opt[_], InternalID <: IDTop](
    coalgebra: K => FIn[K],
    algebraGenerator: LazyForestGenerator.Context[FOut, InternalID] => FIn[Opt[InternalID]] => Opt[
      InternalID])(implicit ct: ClassTag[Opt[InternalID]])
    extends IlazyForest[K, FOut, Opt, InternalID] {

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
