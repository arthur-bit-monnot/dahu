package dahu.maps.memoization

import dahu.maps.OpaqueIntSubset

import scala.reflect.ClassTag

trait Cache[A, B] {
  def get(a: A): Option[B]

  def getOrElse(a: A, b: => B): B

  def getOrElseUpdate(a: A, b: => B): B
}

class ArrayCache[A, B](ev: OpaqueIntSubset[A])(implicit tag: ClassTag[B]) extends Cache[A, B] {
  import ev._
  private val offset: Int = unwrap(ev.first)
  private val size: Int = 1 + unwrap(last) - unwrap(first)
  private def address(a: A): Int = unwrap(a) - offset

  val memory: Array[B] = new Array[B](size)
  val mask: Array[Boolean] = Array.fill[Boolean](size)(false)

  override def get(a: A): Option[B] =
    if(mask(address(a)))
      Some(memory(unwrap(a) - offset))
    else
      None

  override def getOrElse(a: A, b: => B): B = get(a).getOrElse(b)

  override def getOrElseUpdate(a: A, b: => B): B =
    if(mask(address(a))) {
      memory(address(a))
    } else {
      mask(address(a)) = true
      memory(address(a)) = b
      b
    }
}
