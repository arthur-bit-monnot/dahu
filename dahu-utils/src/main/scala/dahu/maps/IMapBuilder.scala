package dahu.maps

import dahu.utils.structures._

class IMapBuilder[@sp A: ClassTag]() {
  private val map = debox.Map[Int, A]()

  def currentKeys: debox.Set[Int] = map.keysSet
  def contains(k: Int): Boolean   = map.contains(k)

  def +=(k: Int, v: A): Unit = {
    require(!map.contains(k))
    map.update(k, v)
  }

  def toImmutableArray(implicit default: Default[A]): ArrayMap[A] = {
    val keys: debox.Set[SubInt] = map.keysSet.cast[SubInt]
    require(keys.toIterable().min >= 0)
    val max  = keys.toIterable().max
    val buff = debox.Buffer.fill(max + 1)(default.apply())
    map.foreach { case (k, v) => buff(k) = v }
    ArrayMap.noCopy(keys, buff)
  }
}
