package dahu.graphs

import cats._
import cats.implicits._
import dahu.utils._
import dahu.graphs.impl._
import dahu.graphs.transformations.Transformation
import dahu.recursion.{Fix, Recursion}

import scala.collection.mutable

trait OpenASG[K, F[_], Opt[_], InternalID <: IDTop] extends ASG[K, F, Opt] { self =>
  override type ID = InternalID

  override def rootedAt(root: K): LazyTree[K, F, Opt, InternalID] = LazyTree(this)(root)

  def getExt(k: K)(implicit F: Functor[Opt]): Opt[F[ID]] =
    F.map(getTreeRoot(k))(internalCoalgebra)

  def getTreeRoot(k: K): Opt[ID]

  def internalCoalgebra(i: ID): F[ID]

  override def fixID: OpenASG[K, F, Opt, SubSubInt[IDTop, Marker]] =
    castIDTo[SubSubInt[IDTop, Marker]]
  override def castIDTo[NewInternalID <: IDTop]: OpenASG[K, F, Opt, NewInternalID] =
    this.asInstanceOf[OpenASG[K, F, Opt, NewInternalID]]

  def forceEvaluation(k: K): Unit = {
    getTreeRoot(k)
  }

  def extensible(implicit sf: SFunctor[F],
                 tn: TreeNode[F],
                 fo: Functor[Opt],
                 ct: ClassTagK[F]): ExtensibleASG[K, F, Opt, _, ID] =
    new ExtensibleASG[K, F, Opt, IDTop, ID](getTreeRoot, internalCoalgebra)

  override def prepro[L](f: L => K): OpenASG[L, F, Opt, InternalID] =
    new OpenASG[L, F, Opt, InternalID] {
      override def getTreeRoot(k: L): Opt[InternalID] = self.getTreeRoot(f(k))
      override def internalCoalgebra(i: InternalID): F[self.ID] = self.internalCoalgebra(i)
    }
  override def flatPrepro[L](f: L => Opt[K])(
      implicit m: Monad[Opt]): OpenASG[L, F, Opt, InternalID] =
    new OpenASG[L, F, Opt, InternalID] {
      override def getTreeRoot(l: L): Opt[InternalID] =
        m.flatMap(f(l))((k: K) => self.getTreeRoot(k))
      override def internalCoalgebra(i: InternalID): F[self.ID] = self.internalCoalgebra(i)
    }

  def mapInternal[G[_]](f: F[ID] => G[ID]): OpenASG[K, G, Opt, ID] =
    InternalMappedLazyForest(this)(f)

  def mapInternalGen[G[_]](
      f: InternalMapGenLazyForest.Context[F, G, ID, IDTop] => F[IDTop] => G[IDTop])(
      implicit F: SFunctor[F],
      OF: Functor[Opt]
  ) = //: IlazyForest[K, G, Opt, _] =
    InternalMapGenLazyForest(this)(f)

  def mapExternal[Opt2[_]](f: Opt[ID] => Opt2[ID]): OpenASG[K, F, Opt2, ID] =
    ExternalMappedLazyForest(this)(f)

  def mapFull[G[_], Opt2[_]](f: FullMapGenLazyForest.Generator[F, G, Opt, Opt2, ID])(
      implicit F: SFunctor[F],
      OF: Functor[Opt]
  ) = //: IlazyForest[K, G, Opt, _] =
    new FullMapGenLazyForest[K, F, G, Opt, Opt2, ID](this)(f)

  def build(id: ID)(implicit F: SFunctor[F], ct: ClassTag[F[Fix[F]]]): Fix[F] = {
    Recursion.ana(i => internalCoalgebra(i))(id)
  }

  def filterInternal(f: ID => Boolean)(
      implicit ev: Opt[InternalID] =:= InternalID): OpenASG[K, F, Option, ID] =
    new OpenASG[K, F, Option, InternalID] {
      override def getTreeRoot(k: K): Option[InternalID] = {
        val root = self.getTreeRoot(k)
        if(f(root))
          Some[ID](ev(root))
        else
          None
      }
      override def internalCoalgebra(i: InternalID): F[InternalID] = self.internalCoalgebra(i)
    }

  def filter(f: F[ID] => Boolean)(
      implicit ev: Opt[InternalID] =:= InternalID): OpenASG[K, F, Option, ID] =
    new OpenASG[K, F, Option, InternalID] {
      override def getTreeRoot(k: K): Option[InternalID] = {
        val root = self.getTreeRoot(k)
        if(f(self.internalCoalgebra(root)))
          Some[ID](ev(root))
        else
          None
      }
      override def internalCoalgebra(i: InternalID): F[InternalID] = self.internalCoalgebra(i)
    }

  def descendantsWithID(root: ID)(implicit tn: TreeNode[F]): Seq[(ID, F[ID])] = {
    val queue = mutable.Stack[ID]()
    val visited = mutable.HashSet[ID]()
    val result = mutable.ArrayBuffer[(ID, F[ID])]()
    queue.push(root)

    while(queue.nonEmpty) {
      val cur = queue.pop()
      if(!visited.contains(cur)) {
        val fcur = internalCoalgebra(cur)
        if(tn.children(fcur).forall(visited)) {
          visited += cur
          result += ((cur, fcur))
        } else {
          queue.push(cur)
          queue.pushAll(tn.children(fcur))
        }
      }
    }
    result.toList
  }
  def descendants(root: ID)(implicit tn: TreeNode[F]): Seq[F[ID]] = {
    val queue = mutable.Stack[ID]()
    val visited = mutable.HashSet[ID]()
    val result = mutable.ArrayBuffer[F[ID]]()
    queue.push(root)

    while(queue.nonEmpty) {
      val cur = queue.pop()
      if(!visited.contains(cur)) {
        val fcur = internalCoalgebra(cur)
        if(tn.children(fcur).forall(visited)) {
          visited += cur
          result += fcur
        } else {
          queue.push(cur)
          queue.pushAll(tn.children(fcur))
        }
      }
    }
    result.toList
  }

  def changedKey[K2](f: K2 => K): OpenASG[K2, F, Opt, self.ID] =
    new OpenASG[K2, F, Opt, self.ID] {
      override def getTreeRoot(k: K2): Opt[self.ID] = self.getTreeRoot(f(k))

      override def internalCoalgebra(i: self.ID): F[self.ID] = self.internalCoalgebra(i)

      override def toString: String = "ILazyForestChangedKey"
    }

  // TODO: reimplement in terms of cataExplicit
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

  def cataWithPreFill[V: ClassTag](f: F[V] => V, preFill: ID => Option[V])(
      implicit T: TreeNode[F],
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
              preFill(cur) match {
                case Some(curValue) =>
                  setValue(cur, curValue)
                case None =>
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

  def cataExplicit[V: ClassTag](ff: (SomeID => V) => F[SomeID] => V)(
      implicit T: TreeNode[F],
      F: SFunctor[F]): LazyMap[K, V, Opt, ID] =
    new LazyMap[K, V, Opt, ID] {
      private val values = debox.Map[ID, V]()

      private def hasValue(i: ID): Boolean = values.contains(i)
      private def getValue(i: ID): V = values(i)
      private def setValue(i: ID, v: V): Unit = values.update(i, v)

      val f = ff((i: SomeID) => getValue(i.asInstanceOf[ID]))

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
                setValue(cur, f(fcur.asInstanceOf[F[SomeID]]))
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

  def transform[I <: IDTop, G[_]](fGen: Transformation[F, G])(
      implicit TN: TreeNode[F],
      F: Functor[Opt],
      SF: SFunctor[F],
      ct: ClassTag[G[I]]
  ): ASG[K, G, Opt] =
    new LazyForestLayer[K, G, Opt, I, self.ID] {
      private val idMap = debox.Map[self.ID, I]()
      private val coalg = BiMap[I, G[I]]()
      private var _nextID = 0
      def nextID(): I = { _nextID += 1; (_nextID - 1).asInstanceOf[I] }
      override def record(fi: G[I]): I = {
        if(!coalg.cocontains(fi)) {
          coalg.add(nextID(), fi)
        }
        coalg.coget(fi)
      }

      val transform: F[I] => G[I] = fGen.transformation(coalg.get(_), record)
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
            val y = record(fy)
            idMap.update(cur, y)
          }
        }
        idMap(id)
      }

      override def getTreeRoot(k: K): Opt[I] = self.getTreeRoot(k).map(fromPreviousId(_))

      override def internalCoalgebra(i: I): G[I] = coalg.get(i)
    }

  def internalBottomUpTopologicalOrder(id: ID)(implicit TN: TreeNode[F]): Iterable[ID] = {
    Graph.topologicalOrderLeavesToRoot[ID, F](id, internalCoalgebra(_), TN.children(_))
  }
}

case object RecursiveTransformation extends Exception

object OpenASG {
  def build[K, FIn[_]: TreeNode: SFunctor, FOut[_], Opt[_]](coalgebra: K => FIn[K])(
      algebraGenerator: LazyForestGenerator.Context[FOut, IDTop] => FIn[Opt[IDTop]] => Opt[IDTop])(
      implicit ct: ClassTag[Opt[IDTop]]): OpenASG[K, FOut, Opt, _] =
    new LazyForestGenerator[K, FIn, FOut, Opt, IDTop](coalgebra, algebraGenerator)

  def ana[K, F[_]: SFunctor: TreeNode](coalgebra: K => F[K]) = { //: IlazyForest[K, F, cats.Id, _] = {
    def algebra(ctx: LazyForestGenerator.Context[F, IDTop]): F[IDTop] => IDTop = ctx.record
    new LazyForestGenerator[K, F, F, cats.Id, IDTop](coalgebra, algebra)
  }

  def anaGen[K, FIn[_]: SFunctor: TreeNode, FOut[_]](
      t: K,
      coalgebra: K => FIn[K],
      algebra: LazyForestGenerator.Context[FOut, IDTop] => FIn[IDTop] => IDTop)
    : OpenASG[K, FOut, cats.Id, _] =
    new LazyForestGenerator[K, FIn, FOut, cats.Id, IDTop](coalgebra, algebra)

}
