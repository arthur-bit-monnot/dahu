package dahu.graphs.autotrans
import cats.Functor
import dahu.graphs._
import dahu.graphs.TreeNode._
import dahu.utils._
import dahu.utils.uoption._
import dahu.utils.ClassTagK._

trait Transformation[F[_]] {

  /** Depth of the transformation lookup.
    * If 0, the transformation can only look at the node.
    * If 1, the transformation can only lookup the children of the node.
    * ...
    * */
  def dependenceDepth: Int
  def transformation[I <: Int](retrieve: I => F[I], record: F[I] => I): F[I] => UOption[F[I]]
}

class AutoTransformation[F[_]: SFunctor: TreeNode](trans: Transformation[F])(
    implicit ct: ClassTagK[F]
) {
  type I <: Int

  private var _nextId = 0

  val transformation: F[I] => UOption[F[I]] = trans.transformation(extract, shallowRecord)

  private val fromKeys = debox.Map[I, F[I]]()
  private val fromVals = debox.Map[F[I], I]()

  private def dependentsOn(i: I): Iterator[I] = {
    fromKeys
      .iterator()
      .filter(_._2.existsChild(_ == i))
      .map(_._1)
  }

  private case class Updated(i: I, distanceToUpdatedChild: Int)

  private val queue = new BinaryHeap[Updated]()
  private def enqueue(i: I, distanceToUpdate: Int): Unit = {
    queue.push(Updated(i, distanceToUpdate), distanceToUpdate)
  }
  private def processQueue(): Unit = {

    while(!queue.isEmpty) {
//      println(s"                 queue: ${queue.toList()}")
      val (up @ Updated(i, dist), _) = queue.pop()
//      println(up)
      val fi = fromKeys(i)
      val res = transformation(fi)
      res match {
        case UNone if dist < trans.dependenceDepth =>
          dependentsOn(i)
            .foreach(j => enqueue(j, dist + 1))
        case UNone      =>
        case USome(fi2) =>
//          println(s"$i: $fi -> $fi2")
          fromKeys.update(i, fi2)
          dependentsOn(i)
            .foreach(j => enqueue(j, 1))
          enqueue(i, 0) // enqueue self
      }
    }

    for((i, fi) <- fromKeys.iterator()) {
//      val ffi = fi.smap(extract)
      assert(transformation(fi) == UNone, s"$i $fi ${fi.children.map(extract)}")
    }

    assert(queue.isEmpty)
  }

  private def nextID(): I = {
    val i = _nextId.asInstanceOf[I]
    assert(fromKeys.size == _nextId)
    _nextId += 1

    assert(!fromKeys.contains(i))
    i
  }
  def getKeyOf(fi: F[I]): Option[I] = fromVals.get(fi)

  private def shallowRecord(fi: F[I]): I = {
    if(!fromVals.contains(fi)) {
      val i = nextID()
//      println(s"recording $i: $fi")
      fromVals.update(fi, i)
      fromKeys.update(i, fi)
      enqueue(i, 0)
    }
    fromVals(fi)
  }

  def deepRecord(fi: F[I]): I = {
    val i = shallowRecord(fi)
    processQueue()
    i
  }

  def extract(i: I): F[I] = fromKeys(i)
}
