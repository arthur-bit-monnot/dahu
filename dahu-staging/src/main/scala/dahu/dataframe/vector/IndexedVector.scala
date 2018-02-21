package dahu.dataframe.vector

import dahu.dataframe.errors.{KeyDuplication, KeyMissing, KeyWronglyIndexed}

final case class IndexedVector[A](vector: Vector[A], index: Map[A, Int])

object IndexedVector {

  def from[A](fa: Vector[A]): IndexedVector[A] =
    IndexedVector(fa, fa.zipWithIndex.toMap)

  implicit val vec: Vec[IndexedVector] = new Vec[IndexedVector] {
    override def empty[A]: IndexedVector[A] = IndexedVector(Vector(), Map())

    override def size[A](fa: IndexedVector[A]): Int = fa.vector.size

    override def at[A](fa: IndexedVector[A], i: Int): A = fa.vector(i)

    override def values[A](fa: IndexedVector[A]): Iterable[A] = fa.vector

    override def updated[A](fa: IndexedVector[A], i: Int, value: A): IndexedVector[A] = {
      val prev = at(fa, i)
      if(value != prev) {
        if(!fa.index.contains(prev)) throw KeyMissing(prev)
        if(fa.index(prev) != i) throw KeyWronglyIndexed(prev)
        if(fa.index.contains(value)) throw KeyDuplication(value)

        val newVector = fa.vector.updated(i, value)
        val newMap = (fa.index - prev) + ((value, i))
        IndexedVector(newVector, newMap)
      } else {
        fa
      }
    }

    override def withAppended[A](fa: IndexedVector[A], value: A): IndexedVector[A] = {
      if(fa.vector.contains(value)) throw KeyDuplication(value)
      val i = fa.vector.size
      IndexedVector(fa.vector :+ value, fa.index + ((value, i)))
    }

  }

  implicit def indexTypeclass[A]: Index[IndexedVector, A] =
    new Index[IndexedVector, A] {
      override def contains(fa: IndexedVector[A], a: A): Boolean =
        fa.index.contains(a)
      override def idUnsafe(fa: IndexedVector[A], a: A): Int = fa.index(a)
    }
}
