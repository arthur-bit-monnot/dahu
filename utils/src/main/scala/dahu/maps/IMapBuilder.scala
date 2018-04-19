package dahu.maps

import dahu.utils.structures._

class IMapBuilder[@sp A: ClassTag]() {
  private val map = debox.Map[Int, A]()

  def currentKeys: debox.Set[Int] = map.keysSet
  def contains(k: Int): Boolean = map.contains(k)

  def get(k: Int): Option[A] = map.get(k)
  def getOrElseUpdate(k: Int, v: => A): A = {
    if(map.contains(k))
      map(k)
    else
      map.getOrElseUpdate(k, v) // split because the underlying collection eagerly evaluates `v`
  }

  def +=(k: Int, v: A): Unit = {
    require(!map.contains(k))
    map.update(k, v)
  }

  def toImmutableArray: ArrayMap[A] = {
    val keys: debox.Set[SubInt] = map.keysSet.cast[SubInt]
    require(keys.toIterable().min >= 0)
    val max = keys.toIterable().max
    val buff = new debox.Buffer(new Array[A](max + 1), max + 1)
    map.foreach { case (k, v) => buff(k) = v }
    ArrayMap.noCopy(keys, buff)
  }
}
