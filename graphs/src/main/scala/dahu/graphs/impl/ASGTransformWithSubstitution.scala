package dahu.graphs.impl

import cats._
import cats.implicits._
import dahu.graphs.{ASG, IDTop, OpenASG, SomeID, TreeNode}
import dahu.utils._
import dahu.graphs.TreeNode._
import dahu.graphs.transformations.TransformationWithSubstitution

import scala.collection.mutable

class ASGTransformWithSubstitution[K,
                                   F[_]: TreeNode: SFunctor,
                                   G[_]: SFunctor,
                                   Opt[_]: Functor,
                                   I <: IDTop,
                                   OI <: IDTop] private (val mapped: OpenASG[K, F, Opt, OI])(
    fExistential: TransformationWithSubstitution[F, G])(implicit ct: ClassTag[G[I]])
    extends OpenASG[K, G, Opt, I] {

  val f: F[I] => (G[I], Option[I => I]) = fExistential.transformation(internalCoalgebra)

  private var _nextID: Int = 0
  def nextID(): I = {
    _nextID += 1
    (_nextID - 1).asInstanceOf[I]
  }
  private val memo = BiMap[I, G[I]]()
  private val idsMap = mutable.Map[OI, I]()
  private def processed(i: OI): Boolean = idsMap.contains(i)

  def record(gi: G[I]): I = {
    if(memo.cocontains(gi)) {
      memo.coget(gi)
    } else {
      val i = nextID()
      memo.add(i, gi)
      i
    }
  }

  private def replace(f: I => I, in: I): I = {
    def recursiveReplace(x: I): I = {
      val fi = internalCoalgebra(x)
      val fi2 = fi.smap(recursiveReplace)
      val resultTmp = record(fi2)
      val result = f(resultTmp)
      result
    }

    recursiveReplace(in)
  }
  private def algebra(fi: F[I]): I = {
    f(fi) match {
      case (gi, None) => record(gi)
      case (gi, Some(trans)) =>
        replace(trans, record(gi))
    }
  }

  override def getTreeRoot(key: K): Opt[ID] = {
    mapped.getTreeRoot(key).map(processOldId)
  }

  private def processOldId(oi: OI): I = {
    if(processed(oi))
      return idsMap(oi)

    val queue = mutable.Stack[OI]()
    queue.push(oi)

    while(queue.nonEmpty) {
      val cur = queue.pop()
      if(!processed(cur)) {
        val fk = mapped.internalCoalgebra(cur)
        if(fk.forallChildren(processed)) {
          val fg = implicitly[SFunctor[F]].smap(fk)(idsMap)
          val g: I = algebra(fg)
          idsMap.update(cur, g)
        } else {
          queue.push(cur)
          fk.foreachChild(queue.push(_))
        }
      }
    }
    idsMap(oi)
  }

  override def internalCoalgebra(i: I): G[I] = {
    assert(memo.contains(i), "An ID was generated without recording its representation")
    memo.get(i)
  }
}

object ASGTransformWithSubstitution {
  def build[K, F[_]: TreeNode: SFunctor, G[_]: SFunctor, Opt[_]: Functor](mapped: ASG[K, F, Opt])(
      transform: TransformationWithSubstitution[F, G])(
      implicit ct: ClassTag[G[IDTop]]): ASG[K, G, Opt] = {
    val x = mapped.fixID
    new ASGTransformWithSubstitution[K, F, G, Opt, IDTop, x.ID](x)(transform)
  }
}
