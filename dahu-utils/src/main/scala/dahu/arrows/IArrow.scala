package dahu.arrows

import dahu.utils.structures.Default

import scala.reflect.ClassTag
import scala.{specialized => sp}

/** TODO: rename to IArrow once refactoring is finished. */
trait IntFunc[@sp V] {
  type T <: SubInt
  type Key = T
  protected def wrap(i: Int): Key              = i.asInstanceOf[Key]
  protected def wrapF[F[_]](f: F[Int]): F[Key] = f.asInstanceOf[F[Key]]

  def domain: debox.Set[Key]
  def apply(key: Key): V
  def isInDomain(i: Int): Boolean = domain(wrap(i))

  def get(key: Int): Option[V] = if(domain(wrap(key))) Some(apply(wrap(key))) else None

  def map[@sp B: ClassTag: Default](f: V => B): IntFunc.Aux[T, B]
}
object IntFunc {
  type Aux[K0, V] = IntFunc[V] { type T = K0 }
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
    val keys: debox.Set[SubInt] = map.keysSet.asInstanceOf[debox.Set[SubInt]]
    require(keys.toIterable().min >= 0)
    val max  = keys.toIterable().max
    val buff = debox.Buffer.fill(max + 1)(default.apply())
    map.foreach { case (k, v) => buff(k) = v }
    ArrayIntFunc.noCopy(keys, buff)
  }
}

trait MutableIntFunc[@sp V] extends IntFunc[V] {
  def update(key: Key, value: V)
}

class MutableMapIntFunc[@sp V: ClassTag] private[arrows] (
    private val mapImpl: debox.Map[Int, V]
) extends MutableIntFunc[V] {

  override def domain             = wrapF(mapImpl.keysSet)
  override def apply(value: T): V = mapImpl(value)

  override def update(key: T, value: V): Unit = {
    assert(mapImpl.contains(key))
    mapImpl.update(key, value)
  }

  def map[@sp B: ClassTag: Default](f: V => B): MutableMapIntFunc.Aux[T, B] =
    new MutableMapIntFunc[B](mapImpl.mapValues(f)).asInstanceOf[MutableMapIntFunc.Aux[T, B]]
}
object MutableMapIntFunc {
  type Aux[K0, B] = MutableMapIntFunc[B] { type T = K0 }
}

class ArrayIntFunc[@sp V: ClassTag] private[arrows] (
    private val keysAsInt: debox.Set[Int], // todo: should be immutable
    private val buff: debox.Buffer[V]
) extends IntFunc[V] {

  private def keys: debox.Set[T] = wrapF(keysAsInt)

  override def domain: debox.Set[Key] = wrapF(keysAsInt.copy())
  override def apply(value: Key): V   = buff(value)

  override def map[@sp B: ClassTag: Default](f: V => B): ArrayIntFunc.Aux[T, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(buff(i)))

    new ArrayIntFunc[B](keysAsInt, newBuff).asInstanceOf[ArrayIntFunc.Aux[T, B]]
  }

  def mapFromKey[@sp B: ClassTag: Default](f: (T, V) => B): ArrayIntFunc.Aux[T, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(i, buff(i)))

    ArrayIntFunc.noCopy(keys, newBuff)
  }

  def filter[TT](predicate: V => Boolean)(implicit ev: TT =:= predicate.type): ArrayIntFunc.Aux[SubSubInt[T,TT], V] = {
    type subKey = SubSubInt[T, TT] // Int with SubInt with T with ev.T
    val subKeys = keys.copy().asInstanceOf[debox.Set[subKey]]
    println(subKeys.toIterable())
    subKeys.filterSelf(i => predicate(apply(i)))
    println(subKeys.toIterable())

//    new ArrayIntFunc(untagged(subKeys), buff) {
//      type T = subKey
//    }
//      .asInstanceOf[ArrayIntFunc.Aux[subKey, V]]
    ArrayIntFunc.noCopy(subKeys, buff)
//      .asInstanceOf[ArrayIntFunc.Aux[T with ev.T, V]]
  }

  def toMutable: MutableArrayIntFunc.Aux[T, V] = MutableArrayIntFunc.noCopy(keys, buff.copy())

  def toIterable: Iterable[(T,V)] = keys.toIterable().map(i => (i, buff(i)))

  override def hashCode(): Int = toIterable.hashCode()

  override def equals(o: Any): Boolean = o match {
    case that: ArrayIntFunc[_] => this.toIterable == that.toIterable
    case _ => false
  }
}
object ArrayIntFunc {
  type Aux[K0, B] = ArrayIntFunc[B] { type T = K0 }

  def noCopy[K <: SubInt, @sp V: ClassTag](
      keys: debox.Set[K],
      buffer: debox.Buffer[V]): ArrayIntFunc.Aux[K, V] =
    new ArrayIntFunc[V](untagged(keys), buffer)
      .asInstanceOf[ArrayIntFunc.Aux[K, V]]

  def build[@sp V: Default: ClassTag](domain: Iterable[Int], f: Int => V): ArrayIntFunc[V] = {
    val keys = debox.Set.fromIterable(domain)
    val size = domain.max +1
    assert(domain.min >= 0)
    val default = Default.of[V]
    val buff = debox.Buffer.fill(size)(default)
    keys.foreach(i => buff(i) = f(i))
    new ArrayIntFunc[V](keys, buff)
  }
}

class MutableArrayIntFunc[@sp V: ClassTag] private[arrows] (
    private val intKeys: debox.Set[Int], // todo: should be immutable
    private val buff: debox.Buffer[V]
) extends MutableIntFunc[V] {
  def keys: debox.Set[T] = wrapF(intKeys)

  override def domain: debox.Set[Key] = keys.copy()
  override def apply(value: Key): V   = buff(value)

  override def update(key: Key, value: V): Unit = buff(key) = value

  override def map[@sp B: ClassTag: Default](f: V => B): MutableArrayIntFunc.Aux[T, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(buff(i)))

    MutableArrayIntFunc.noCopy(keys, newBuff)
  }

  def toImmutable: ArrayIntFunc.Aux[T, V] =
    ArrayIntFunc.noCopy(keys, buff.copy())
}
object MutableArrayIntFunc {
  type Aux[K0, V] = MutableArrayIntFunc[V] { type T = K0 }

  def apply[K0 <: SubInt, A: ClassTag](): MutableArrayIntFunc.Aux[K0, A] =
    new MutableArrayIntFunc(debox.Set(), debox.Buffer[A]())
      .asInstanceOf[MutableArrayIntFunc.Aux[K0, A]] // cast useful to avoid creating anonymous classes

  def noCopy[K0 <: SubInt, V: ClassTag](
      keys: debox.Set[K0],
      values: debox.Buffer[V]): MutableArrayIntFunc.Aux[K0, V] =
    new MutableArrayIntFunc[V](untagged(keys), values)
      .asInstanceOf[MutableArrayIntFunc.Aux[K0, V]] // cast to avoid creating anonymous classes
}
