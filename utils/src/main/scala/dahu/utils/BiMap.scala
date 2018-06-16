package dahu.utils

import scala.collection.mutable

class BiMap[A, B] private (forward: mutable.Map[A, B], backward: mutable.Map[B, A]) {

  def size: Int = forward.size

  def add(a: A, b: B): Unit = {
    require(!forward.contains(a))
    require(!backward.contains(b))
    forward += ((a, b))
    backward += ((b, a))
  }

  def get(a: A): B = forward(a)
  def coget(b: B): A = backward(b)

  def contains(a: A): Boolean = forward.contains(a)
  def cocontains(b: B): Boolean = backward.contains(b)

  def iterator: Iterator[(A, B)] = forward.iterator
}

object BiMap {
  def apply[A, B](): BiMap[A, B] = new BiMap(mutable.HashMap[A, B](), mutable.HashMap[B, A]())
}
