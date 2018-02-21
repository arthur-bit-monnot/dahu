package dahu.maps.mutable

import dahu.maps._
import dahu.utils.structures.Default

final class MIMap[@sp V: ClassTag] private[maps] (
    private val mapFromInt: debox.Map[Int, V]
) extends SharedIMap {

  def content: debox.Map[K, V] = mapFromInt.asInstanceOf[debox.Map[K, V]]

  def domain = wrapF(mapFromInt.keysSet)
  def apply(value: K): V = mapFromInt(value)

  def update(key: K, value: V): Unit = {
    assert(mapFromInt.contains(key))
    mapFromInt.update(key, value)
  }

  def map[@sp B: ClassTag: Default](f: V => B): MIMap.Aux[K, B] =
    MIMap.noCopy(content.mapValues(f))

  def castKey[K2 <: SubInt]: MIMap.Aux[K2, V] =
    this.asInstanceOf[MIMap.Aux[K2, V]]
}
object MIMap {
  type Aux[K0, B] = MIMap[B] { type K = K0 }

  def noCopy[K <: SubInt, V: ClassTag](content: debox.Map[K, V]): MIMap.Aux[K, V] =
    new MIMap(content.asInstanceOf[debox.Map[Int, V]])
      .castKey[K]
}
