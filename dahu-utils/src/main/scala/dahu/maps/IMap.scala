package dahu.maps

import dahu.utils.structures._

/** Trait that is shared between mutable and immutable instances. */
trait SharedIMap {
  type K <: SubInt
  protected def wrap(i: Int): K = i.asInstanceOf[K]
  protected def wrapF[F[_]](f: F[Int]): F[K] = f.asInstanceOf[F[K]]
}

trait IMap[@sp V] extends SharedIMap {
  def domain: debox.Set[K]
  def apply(key: K): V
  def isInDomain(i: Int): Boolean = domain(wrap(i))

  /** Get by a key which has a supertype of K. This is safe bu the key might not be present. */
  def get[UK >: K <: SubInt](key: UK): Option[V] =
    if(domain(wrap(key))) Some(apply(wrap(key))) else None

  /** This is highly unsafe since keys might have been used interchangeably with other IMaps */
  def getUnsafe(key: Int): Option[V] = if(domain(wrap(key))) Some(apply(wrap(key))) else None

  def map[@sp B: ClassTag: Default](f: V => B): IMap.Aux[K, B]
}
object IMap {
  type Aux[K0, V] = IMap[V] { type K = K0 }
}
