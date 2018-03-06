package dahu.maps.growable

import dahu.maps.{ArrayMap, ClassTag, SharedIMap, SubInt}

import scala.{specialized => sp}

final class GrowableBiMap[@sp V: ClassTag] private[maps] (
    private val mapFromInt: debox.Map[Int, V],
    private val reverse: debox.Map[V, Int],
    private val keys: debox.Set[Int]
) extends SharedIMap {

  def content: debox.Map[K, V] = mapFromInt.asInstanceOf[debox.Map[K, V]]
  def reverseContent: debox.Map[V, K] = reverse.asInstanceOf[debox.Map[V, K]]

  def domain = wrapF(keys.copy())
  def apply(value: K): V = mapFromInt(value)

  def keyOf(v: V): K = {
    reverseContent.get(v) match {
      case Some(k) => k
      case None    =>
        // generate a new unique key
        val k = (content.keys.asInstanceOf[Array[Int]].max + 1).asInstanceOf[K] // todo: O(N)
        assert(!content.contains(k))
        assert(!keys(k))
        keys.add(k)
        reverse.update(v, k)
        content.update(k, v)
        k
    }
  }
  def update(key: K, value: V): Unit = {
    assert(content.contains(key))
    assert(keys(key))
    content.update(key, value)
    reverseContent.update(value, key)
  }

  def toImmutableArray: ArrayMap.Aux[K, V] = ArrayMap.buildWithKey(domain)(asFunction)

  def asFunction: K => V = content(_)
}

object GrowableBiMap {
  def empty[V: ClassTag](): GrowableBiMap[V] =
    new GrowableBiMap[V](debox.Map[Int, V](), debox.Map[V, Int](), debox.Set())

  def fromArray[K <: SubInt, A, V: ClassTag](orig: ArrayMap.Aux[K, A])(
      f: A => V): GrowableBiMap[V] = {
    val base = debox.Map[Int, V]()
    for(id <- orig.domain) {
      base.update(id, f(orig(id)))
    }
    val reverse = debox.Map[V, Int]()
    for(k <- base.keys) {
      reverse.update(base(k), k)
    }
    new GrowableBiMap[V](base, reverse, orig.domain.asInstanceOf[debox.Set[Int]])
  }
}
//object MIMap {
//  type Aux[K0, B] = MIMap[B] { type K = K0 }
//
//  def noCopy[K <: SubInt, V: ClassTag](content: debox.Map[K, V]): MIMap.Aux[K, V] =
//    new MIMap(content.asInstanceOf[debox.Map[Int, V]])
//      .castKey[K]
//}
