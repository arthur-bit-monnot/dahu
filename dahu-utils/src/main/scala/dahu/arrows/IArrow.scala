package dahu.arrows

import dahu.utils.structures._

import scala.reflect.ClassTag
import scala.{specialized => sp}

/** TODO: rename to IntMap once refactoring is finished. */
trait IntFunc[@sp V] {
  type K <: SubInt
  protected def wrap(i: Int): K              = i.asInstanceOf[K]
  protected def wrapF[F[_]](f: F[Int]): F[K] = f.asInstanceOf[F[K]]

  def domain: debox.Set[K]
  def apply(key: K): V
  def isInDomain(i: Int): Boolean = domain(wrap(i))

  def get(key: Int): Option[V] = if(domain(wrap(key))) Some(apply(wrap(key))) else None

  def map[@sp B: ClassTag: Default](f: V => B): IntFunc.Aux[K, B]
}
object IntFunc {
  type Aux[K0, V] = IntFunc[V] { type K = K0 }
}

class IntFuncBuilder[@sp A: ClassTag]() {
  private val map = debox.Map[Int, A]()

  def currentKeys: debox.Set[Int] = map.keysSet
  def contains(k: Int): Boolean   = map.contains(k)

  def +=(k: Int, v: A): Unit = {
    require(!map.contains(k))
    map.update(k, v)
  }

  def toImmutableArray(implicit default: Default[A]): ArrayIntFunc[A] = {
    val keys: debox.Set[SubInt] = map.keysSet.cast[SubInt]
    require(keys.toIterable().min >= 0)
    val max  = keys.toIterable().max
    val buff = debox.Buffer.fill(max + 1)(default.apply())
    map.foreach { case (k, v) => buff(k) = v }
    ArrayIntFunc.noCopy(keys, buff)
  }
}

trait MutableIntFunc[@sp V] extends IntFunc[V] {
  def update(key: K, value: V)
}

final class MutableMapIntFunc[@sp V: ClassTag] private[arrows] (
    private val mapFromInt: debox.Map[Int, V]
) extends MutableIntFunc[V] {

  def content: debox.Map[K, V] = mapFromInt.asInstanceOf[debox.Map[K, V]]

  override def domain             = wrapF(mapFromInt.keysSet)
  override def apply(value: K): V = mapFromInt(value)

  override def update(key: K, value: V): Unit = {
    assert(mapFromInt.contains(key))
    mapFromInt.update(key, value)
  }

  def map[@sp B: ClassTag: Default](f: V => B): MutableMapIntFunc.Aux[K, B] =
    MutableMapIntFunc.noCopy(content.mapValues(f))

  def castKey[K2 <: SubInt]: MutableMapIntFunc.Aux[K2, V] =
    this.asInstanceOf[MutableMapIntFunc.Aux[K2, V]]
}
object MutableMapIntFunc {
  type Aux[K0, B] = MutableMapIntFunc[B] { type K = K0 }

  def noCopy[K <: SubInt, V: ClassTag](content: debox.Map[K, V]): MutableMapIntFunc.Aux[K, V] =
    new MutableMapIntFunc(content.asInstanceOf[debox.Map[Int, V]])
      .castKey[K]
}

class ArrayIntFunc[@sp V: ClassTag] private[arrows] (
    private val keysAsInt: debox.Set[Int], // todo: should be immutable
    private val buff: debox.Buffer[V] // todo: should be immutable
) extends IntFunc[V] {

  private def keys: debox.Set[K] = wrapF(keysAsInt)

  override def domain: debox.Set[K] = wrapF(keysAsInt.copy())
  override def apply(value: K): V   = buff(value)

  def asFunction: K => V = apply

  override def map[@sp B: ClassTag: Default](f: V => B): ArrayIntFunc.Aux[K, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(buff(i)))
    new ArrayIntFunc[B](keysAsInt, newBuff).asInstanceOf[ArrayIntFunc.Aux[K, B]]
  }

  def mapFromKey[@sp B: ClassTag: Default](f: (K, V) => B): ArrayIntFunc.Aux[K, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(i, buff(i)))

    ArrayIntFunc.noCopy(keys, newBuff)
  }

  def filter[TT](predicate: V => Boolean)(
      implicit ev: TT =:= predicate.type): ArrayIntFunc.Aux[SubSubInt[K, TT], V] = {
    type SubKey = SubSubInt[K, TT]
    val subKeys = keys.findAll(i => predicate(apply(i))).cast[SubKey]
    ArrayIntFunc.noCopy(subKeys, buff)
  }

  def collect[TT, @sp B: ClassTag: Default](pf: PartialFunction[V, B])(
      implicit ev: TT =:= pf.type): ArrayIntFunc.Aux[SubSubInt[K, TT], B] = {
    type SubKey = SubSubInt[K, TT]
    val subKeys: debox.Set[SubKey] = keys.findAll(i => pf.isDefinedAt(apply(i))).cast[SubKey]
    val default                    = Default.of[B]
    val newBuff                    = debox.Buffer.fill(buff.length)(default)
    subKeys.foreach(i => newBuff(i) = pf.apply(buff(i)))
    ArrayIntFunc.noCopy(subKeys, newBuff)
  }

  def castKey[K2 <: SubInt]: ArrayIntFunc.Aux[K2, V] = {
    this.asInstanceOf[ArrayIntFunc.Aux[K2, V]]
  }

  def toMutable: MutableArrayIntFunc.Aux[K, V] = MutableArrayIntFunc.noCopy(keys, buff.copy())

  def toIterable: Iterable[(K, V)] = keys.toIterable().map(i => (i, buff(i)))

  override def hashCode(): Int = toIterable.hashCode()

  override def equals(o: Any): Boolean = o match {
    case that: ArrayIntFunc[_] => this.toIterable == that.toIterable
    case _                     => false
  }
}
object ArrayIntFunc {
  type Aux[K0, B] = ArrayIntFunc[B] { type K = K0 }

  def noCopy[K <: SubInt, @sp V: ClassTag](keys: debox.Set[K],
                                           buffer: debox.Buffer[V]): ArrayIntFunc.Aux[K, V] =
    new ArrayIntFunc[V](untagged(keys), buffer)
      .castKey[K]

  def build[@sp V: Default: ClassTag](domain: Iterable[Int], f: Int => V): ArrayIntFunc[V] = {
    val keys = debox.Set.fromIterable(domain)
    val size = domain.max + 1
    assert(domain.min >= 0)
    val default = Default.of[V]
    val buff    = debox.Buffer.fill(size)(default)
    keys.foreach(i => buff(i) = f(i))
    new ArrayIntFunc[V](keys, buff)
  }
}

class MutableArrayIntFunc[@sp V: ClassTag] private[arrows] (
    private val intKeys: debox.Set[Int], // todo: should be immutable
    private val buff: debox.Buffer[V]
) extends MutableIntFunc[V] {
  def keys: debox.Set[K] = wrapF(intKeys)

  override def domain: debox.Set[K] = keys.copy()
  override def apply(value: K): V   = buff(value)

  override def update(key: K, value: V): Unit = buff(key) = value

  override def map[@sp B: ClassTag: Default](f: V => B): MutableArrayIntFunc.Aux[K, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(buff(i)))

    MutableArrayIntFunc.noCopy(keys, newBuff)
  }

  def toImmutable: ArrayIntFunc.Aux[K, V] =
    ArrayIntFunc.noCopy(keys, buff.copy())

  def castKey[K2 <: SubInt]: MutableArrayIntFunc.Aux[K2, V] =
    this.asInstanceOf[MutableArrayIntFunc.Aux[K2, V]]
}
object MutableArrayIntFunc {
  type Aux[K0, V] = MutableArrayIntFunc[V] { type K = K0 }

  def noCopy[K0 <: SubInt, V: ClassTag](keys: debox.Set[K0],
                                        values: debox.Buffer[V]): MutableArrayIntFunc.Aux[K0, V] =
    new MutableArrayIntFunc[V](untagged(keys), values)
      .castKey[K0]
}
