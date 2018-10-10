package dahu.graphs

import cats.Functor

trait LazyMap[K, V, Opt[_], I] {
  def get(k: K)(implicit F: Functor[Opt]): Opt[V]
  def getInternal(i: I): V

  def asFunction(implicit F: Functor[Opt]): K => Opt[V] = get
  def asInternalFunction: I => V = getInternal
}
