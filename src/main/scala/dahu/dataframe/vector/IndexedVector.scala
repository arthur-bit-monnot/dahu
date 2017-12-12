package dahu.dataframe.vector

final case class IndexedVector[A](v: Vector[A], index: Map[A, Int])

object IndexedVector {

  sealed abstract class IndexingError(msg: String) extends Throwable(msg)
  final case class KeyDuplication[A](key: A)
      extends IndexingError(s"Key $key is duplicated in an indexed vector")
  final case class KeyMissing[A](key: A)
      extends IndexingError(
        s"Indexing error: value $key is not present in the index an indexed vector")
  final case class KeyWronglyIndexed[A](key: A)
      extends IndexingError(
        s"Indexing error: value $key is not present in the index an indexed vector")

  implicit def vec[A]: Vec[IndexedVector, A] = new Vec[IndexedVector, A] {
    override def empty: IndexedVector[A] = IndexedVector(Vector(), Map())

    override def size(fa: IndexedVector[A]): Int = fa.v.size

    override def at(fa: IndexedVector[A], i: Int): A = fa.v(i)

    override def updated(fa: IndexedVector[A],
                         i: Int,
                         value: A): IndexedVector[A] = {
      val prev = at(fa, i)
      if (value != prev) {
        if (!fa.index.contains(prev)) throw KeyMissing(prev)
        if (fa.index(prev) != i) throw KeyWronglyIndexed(prev)
        if (fa.index.contains(value)) throw KeyDuplication(value)

        val newVector = fa.v.updated(i, value)
        val newMap = (fa.index - prev) + ((value, i))
        IndexedVector(newVector, newMap)
      } else {
        fa
      }
    }

    override def withAppended(fa: IndexedVector[A],
                              value: A): IndexedVector[A] = {
      if (fa.v.contains(value)) throw KeyDuplication(value)
      val i = fa.v.size
      IndexedVector(fa.v :+ value, fa.index + ((value, i)))
    }
  }

  implicit def indexTypeclass[A]: Index[IndexedVector, A] = new Index[IndexedVector, A] {
    override def contains(fa: IndexedVector[A], a: A): Boolean = fa.index.contains(a)
    override def idUnsafe(fa: IndexedVector[A], a: A): Int = fa.index(a)
  }
}
