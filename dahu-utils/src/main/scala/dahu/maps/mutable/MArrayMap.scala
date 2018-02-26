package dahu.maps.mutable

import dahu.maps._
import dahu.utils.structures._

class MArrayMap[@sp V: ClassTag] private[maps] (
    private val intKeys: debox.Set[Int], // todo: should be immutable
    private val buff: debox.Buffer[V]
) extends SharedIMap {
  def hasKey(k: Int): Boolean = intKeys(k)
  def keys: debox.Set[K] = wrapF(intKeys)

  def domain: debox.Set[K] = keys.copy()
  def apply(value: K): V = buff(value)

  def update(key: K, value: V): Unit = buff(key) = value

  def map[@sp B: ClassTag: Default](f: V => B): MArrayMap.Aux[K, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(buff(i)))

    MArrayMap.noCopy(keys, newBuff)
  }

  def toImmutable: ArrayMap.Aux[K, V] =
    ArrayMap.noCopy(keys, buff.copy())

  def castKey[K2 <: SubInt]: MArrayMap.Aux[K2, V] =
    this.asInstanceOf[MArrayMap.Aux[K2, V]]
}
object MArrayMap {
  type Aux[K0, V] = MArrayMap[V] { type K = K0 }

  def noCopy[K0 <: SubInt, V: ClassTag](keys: debox.Set[K0],
                                        values: debox.Buffer[V]): MArrayMap.Aux[K0, V] =
    new MArrayMap[V](untagged(keys), values)
      .castKey[K0]
}
