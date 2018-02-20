package dahu.maps

import dahu.maps.mutable.MArrayMap
import dahu.utils.structures._

class ArrayMap[@sp V: ClassTag] private[maps] (
    private val keysAsInt: debox.Set[Int], // todo: should be immutable
    private val buff: debox.Buffer[V] // todo: should be immutable
) extends IMap[V] {

  private def keys: debox.Set[K] = wrapF(keysAsInt)

  override def domain: debox.Set[K] = wrapF(keysAsInt.copy())
  override def apply(value: K): V   = buff(value)

  def asFunction: K => V = apply

  override def map[@sp B: ClassTag: Default](f: V => B): ArrayMap.Aux[K, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(buff(i)))
    new ArrayMap[B](keysAsInt, newBuff).asInstanceOf[ArrayMap.Aux[K, B]]
  }

  def mapFromKey[@sp B: ClassTag: Default](f: (K, V) => B): ArrayMap.Aux[K, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(i, buff(i)))

    ArrayMap.noCopy(keys, newBuff)
  }

  def filter[TT](predicate: V => Boolean)(
      implicit ev: TT =:= predicate.type): ArrayMap.Aux[SubSubInt[K, TT], V] = {
    type SubKey = SubSubInt[K, TT]
    val subKeys = keys.findAll(i => predicate(apply(i))).cast[SubKey]
    ArrayMap.noCopy(subKeys, buff)
  }

  def collect[TT, @sp B: ClassTag: Default](pf: PartialFunction[V, B])(
      implicit ev: TT =:= pf.type): ArrayMap.Aux[SubSubInt[K, TT], B] = {
    type SubKey = SubSubInt[K, TT]
    val subKeys: debox.Set[SubKey] = keys.findAll(i => pf.isDefinedAt(apply(i))).cast[SubKey]
    val default                    = Default.of[B]
    val newBuff                    = debox.Buffer.fill(buff.length)(default)
    subKeys.foreach(i => newBuff(i) = pf.apply(buff(i)))
    ArrayMap.noCopy(subKeys, newBuff)
  }

  def castKey[K2 <: SubInt]: ArrayMap.Aux[K2, V] = {
    this.asInstanceOf[ArrayMap.Aux[K2, V]]
  }

  def toMutable: MArrayMap.Aux[K, V] = MArrayMap.noCopy(keys, buff.copy())

  def toIterable: Iterable[(K, V)] = keys.toIterable().map(i => (i, buff(i)))

  override def hashCode(): Int = toIterable.hashCode()

  override def equals(o: Any): Boolean = o match {
    case that: ArrayMap[_] => this.toIterable == that.toIterable
    case _                 => false
  }
}
object ArrayMap {
  type Aux[K0, B] = ArrayMap[B] { type K = K0 }

  def noCopy[K <: SubInt, @sp V: ClassTag](keys: debox.Set[K],
                                           buffer: debox.Buffer[V]): ArrayMap.Aux[K, V] =
    new ArrayMap[V](untagged(keys), buffer)
      .castKey[K]

  def build[@sp V: Default: ClassTag](domain: Iterable[Int], f: Int => V): ArrayMap[V] = {
    val keys = debox.Set.fromIterable(domain)
    val size = domain.max + 1
    assert(domain.min >= 0)
    val default = Default.of[V]
    val buff    = debox.Buffer.fill(size)(default)
    keys.foreach(i => buff(i) = f(i))
    new ArrayMap[V](keys, buff)
  }
}
