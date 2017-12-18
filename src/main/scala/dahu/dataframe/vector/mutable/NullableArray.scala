package dahu.dataframe.vector.mutable

import java.{util => ju}

import scala.reflect.ClassTag

class NullableArray[@specialized(Int,Double) A](mask: ju.BitSet, values: Array[A]) {
  require(mask.size() >= values.length)


  def size: Int = values.length

  def isSet(i: Int): Boolean = mask.get(i)
  def set(i: Int, value: A): Unit = {
    require(value != null)
    mask.set(i)
    values(i) = value
  }
  def unset(i: Int): Unit = mask.clear(i)

  def get(i: Int): Option[A] = if(mask.get(i)) Some(values(i)) else None
  def getUnsafe(i: Int): A = values(i)

  def firstSet: Int = mask.nextSetBit(0)
  def firstUnset: Int = mask.nextClearBit(0)

  def nullCount: Int = size - mask.cardinality()

  def asVector: Option[Vector[A]] = if(nullCount == 0) Some(values.toVector) else None
}

object NullableArray {

  def ofSize[A: ClassTag](size: Int) = new NullableArray[A](new ju.BitSet(size), new Array(size))

}


