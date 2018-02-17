package dahu.structures

import debox._
import dahu.recursion.Types._
import dahu.utils.structures.Default

import scala.reflect.ClassTag

trait IntFunc[T, @specialized V] {
  type Key = KI[T]
  protected def wrap(i: Int): Key              = i.asInstanceOf[Key]
  protected def wrapF[F[_]](f: F[Int]): F[Key] = f.asInstanceOf[F[Key]]

  def domain: debox.Set[Key]
  def apply(key: Key): V
  def isInDomain(i: Int): Boolean = domain(wrap(i))

  def get(key: Int): Option[V] = if(domain(wrap(key))) Some(apply(wrap(key))) else None

  def map[@specialized B: ClassTag: Default](f: V => B): IntFunc[T, B]
}

class IntFuncBuilder[@specialized A: ClassTag]() {
  private val map = debox.Map[Int, A]()

  def currentKeys: debox.Set[Int] = map.keysSet
  def contains(k: Int): Boolean   = map.contains(k)

  def +=(k: Int, v: A): Unit = {
    require(!map.contains(k))
    map.update(k, v)
  }

  def toImmutableArray[T](implicit default: Default[A]): ArrayIntFunc[T, A] = {
    val keys: debox.Set[Int] = map.keysSet
    require(keys.toIterable().min >= 0)
    val max  = keys.toIterable().max
    val buff = debox.Buffer.fill(max + 1)(default.apply())
    map.foreach { case (k, v) => buff(k) = v }
    new ArrayIntFunc[T, A](keys.asInstanceOf[debox.Set[KI[T]]], buff)
  }
}

trait MutableIntFunc[T, @specialized V] extends IntFunc[T, V] {
  // todo: remove and provide builder instances instead.
  // extending allows to by pass type safety, as there is no guarantee that two IntFunc with the same Key type share the same set of keys
//  def extend(key: Int, value: V)
  def update(key: Key, value: V)
}

class MutableMapIntFunc[T, @specialized V: ClassTag] private[structures] (
    private val mapImpl: debox.Map[KI[T], V]
) extends MutableIntFunc[T, V] {

  override def domain               = mapImpl.keysSet
  override def apply(value: Key): V = mapImpl(value)
//  override def extend(key: Int, value: V): Unit = {
//    assert(!mapImpl.contains(wrap(key)))
//    mapImpl.update(wrap(key), value)
//  }
  override def update(key: Key, value: V): Unit = {
    assert(mapImpl.contains(key))
    mapImpl.update(key, value)
  }

  def map[@specialized B: ClassTag: Default](f: V => B): MutableMapIntFunc[T, B] =
    new MutableMapIntFunc[T, B](mapImpl.mapValues(f))
}
object MutableMapIntFunc {
  def apply[T, V: ClassTag](): MutableMapIntFunc[T, V] = new MutableMapIntFunc(debox.Map())
}

class ArrayIntFunc[T, @specialized V: ClassTag: Default] private[structures] (
    private val keys: debox.Set[KI[T]], // todo: should be immutable
    private val buff: debox.Buffer[V]
) extends IntFunc[T, V] {

  override def domain: debox.Set[Key] = keys.copy()
  override def apply(value: Key): V   = buff(value)

  override def map[@specialized B: ClassTag: Default](f: V => B): ArrayIntFunc[T, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(buff(i)))

    new ArrayIntFunc[T, B](keys, newBuff)
  }

  def mapFromKey[@specialized B: ClassTag: Default](f: (Key, V) => B): ArrayIntFunc[T, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(i, buff(i)))

    new ArrayIntFunc[T, B](keys, newBuff)
  }

  def toMutable: MutableArrayIntFunc[T, V] = new MutableArrayIntFunc[T, V](keys.copy(), buff.copy())
}

class MutableArrayIntFunc[T, @specialized V: ClassTag: Default] private[structures] (
    private val keys: debox.Set[KI[T]], // todo: should be immutable
    private val buff: debox.Buffer[V]
) extends MutableIntFunc[T, V] {

  override def domain: debox.Set[Key] = keys.copy()
  override def apply(value: Key): V   = buff(value)

//  override def extend(key: Int, value: V): Unit = {
//    if(!keys.add(wrap(key)))
//      throw new IllegalArgumentException(s"Key $key is already recorded.")
//
//    val defaultValue = Default.of[V]
//    while(buff.length <= key) buff.append(defaultValue)
//    buff(key) = value
//  }

  override def update(key: Key, value: V): Unit = buff(key) = value

  override def map[@specialized B: ClassTag: Default](f: V => B): MutableArrayIntFunc[T, B] = {
    val default = Default.of[B]
    val newBuff = debox.Buffer.fill(buff.length)(default)
    keys.foreach(i => newBuff(i) = f(buff(i)))

    new MutableArrayIntFunc[T, B](keys.copy(), newBuff)
  }

  def toImmutable: ArrayIntFunc[T, V] = new ArrayIntFunc[T, V](keys, buff.copy())
}
object MutableArrayIntFunc {
  def apply[T, A: ClassTag: Default](): MutableArrayIntFunc[T, A] =
    new MutableArrayIntFunc(debox.Set(), debox.Buffer())
}
