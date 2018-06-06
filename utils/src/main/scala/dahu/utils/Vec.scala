package dahu.utils

import spire.algebra._
import spire.math.QuickSort
import spire.syntax.all._

import scala.collection.mutable

final class Vec[@sp A](private val elems: Array[A])(implicit val ct: ClassTag[A])
    extends Serializable { lhs =>

  /**
    * Check if two Buffers are equal.
    *
    * Equal means the buffers have the same type (which is checked
    * using the ClassTag instances) and the same contents.
    *
    * Comparing Buffers with any of Scala's collection types will
    * return false.
    */
  override def equals(that: Any): Boolean = that match {
    case b: Vec[_] =>
      if(length != b.length || ct != b.ct) return false
      val buf = b.asInstanceOf[Vec[A]]
      val limit = length
      cfor(0)(_ < limit, _ + 1) { i =>
        if(elems(i) != buf.elems(i)) return false
      }
      true
    case _ =>
      false
  }

  // TODO: this might be problematic with `equals` that take the classtag into account
  def upcast[B >: A]: Vec[B] = this.asInstanceOf[Vec[B]]

  /**
    * Hash the contents of the buffer to an Int value.
    */
  override def hashCode: Int = {
    var code: Int = 0xf457f00d
    val limit = length
    cfor(0)(_ < limit, _ + 1) { i =>
      code = (code * 19) + elems(i).##
    }
    code
  }

  /**
    * Return a string representation of the contents of the buffer.
    */
  override def toString: String = mkString("[", ", ", "]")

  def mkString(sep: String): String = mkString("", sep, "")
  def mkString(start: String, sep: String, end: String): String = {
    val sb = new StringBuilder()
    sb.append(start)
    cfor(0)(_ < length - 1, _ + 1) { i =>
      sb.append(apply(i))
      sb.append(sep)
    }
    if(length > 0)
      sb.append(apply(length - 1))
    sb.append(end)
    sb.toString()
  }

  /**
    * Return the length of this Buffer as an Int.
    *
    * Since Buffers wrap arrays, their size is limited to what a 32-bit
    * signed integer can represent. In general Buffer should only be
    * used for sequences that are small enough to easily fit into
    * contiguous memory--larger sequences would benefit from block
    * layouts, serialization to/from disk, and other strategies that
    * Buffer does not provide.
    *
    * This is an O(1) operation.
    */
  @inline def length: Int = elems.length
  @inline def size: Int = elems.length

  @inline def indices: Range = 0 until length

  /**
    * Return true if the Buffer is empty, false otherwise.
    *
    * This is an O(1) operation.
    */
  @inline def isEmpty: Boolean = length == 0

  /**
    * Return true if the Buffer is non-empty, false otherwise.
    *
    * This is an O(1) operation.
    */
  @inline def nonEmpty: Boolean = length != 0

  /**
    * Return the value at element i.
    *
    * As noted above, this method may throw an
    * ArrayIndexOutOfBoundsException if i is too large. However, if i
    * is larger than the buffer's length, but fits in the underlying
    * array, a garbage value will be returned instead. Be careful!
    *
    * This is an O(1) operation.
    */
  @inline def apply(i: Int): A = elems(i)

  /**
    * This is a synonym for ++.
    */
  def concat(buf: Vec[A]): Vec[A] = this ++ buf

  /**
    * Concatenate two buffers, returning a new buffer.
    *
    * This method does not modify either input buffer, but allocates
    * and returns a new one.
    *
    * This is an O(n+m) operation, where n and m are the lengths of the
    * input buffers.
    */
  def ++(buf: Vec[A]): Vec[A] = {
    val l1 = length
    val l2 = buf.length
    val out = new Array[A](l1 + l2)
    cfor(0)(_ < l1, _ + 1) { i =>
      out(i) = this(i)
    }
    cfor(0)(_ < l2, _ + 1) { i =>
      out(l1 + i) = buf(i)
    }
    Vec.unsafe(out)
  }

  def :+(a: A): Vec[A] = {
    val l = length
    val out = new Array[A](length + 1)
    cfor(0)(_ < l, _ + 1) { i =>
      out(i) = this(i)
    }
    out(length) = a
    Vec.unsafe(out)
  }

  /**
    * Return a new buffer which consists of the elements [i, j).
    *
    * The slice is half-open: the resulting buffer will include element
    * i but not element j. In other words, the new buffer will have
    * length (j - i).
    *
    * If i and j are not valid indices in the buffer, or if i > j, this
    * method will throw an exception.
    *
    * This is an O(j - i) operation.
    */
  def slice(i: Int, j: Int): Vec[A] = {
    if(0 > i || i > j || j > length)
      throw new IllegalArgumentException("(%s, %s)" format (i, j))
    val n = j - i
    val arr = new Array[A](n)
    System.arraycopy(elems, i, arr, 0, n)
    Vec.unsafe(arr)
  }

  /**
    * Return a new buffer with this buffer's elements in reverse order.
    *
    * This is an O(n) method, where n is buffer.length.
    */
  def reverse(): Vec[A] = {
    val arr = new Array[A](length)
    var i = 0
    var j = length - 1
    val limit = length
    while(i < limit) {
      arr(j) = elems(i)
      i += 1
      j -= 1
    }
    Vec.unsafe(arr)
  }

  /**
    * Return an iterator over this vec's contents.
    *
    * Use this.copy.iterator to get a "clean" iterator if needed.
    *
    * Creating the iterator is an O(1) operation.
    */
  def iterator(): Iterator[A] =
    elems.iterator

  /**
    * Loop over the buffer's contents, appying f to each element.
    *
    * This is an O(n) operation, where n is the length of the buffer.
    */
  def foreach(f: A => Unit): Unit = {
    val limit = length
    cfor(0)(_ < limit, _ + 1) { i =>
      f(elems(i))
    }
  }

  /**
    * Map this buffer's contents into a new buffer using f.
    *
    * This is an O(n) operation, where n is the length of the buffer.
    */
  def map[@sp B: ClassTag](f: A => B): Vec[B] = {
    val arr = new Array[B](length)
    val limit = length
    cfor(0)(_ < limit, _ + 1) { i =>
      arr(i) = f(elems(i))
    }
    Vec.unsafe(arr)
  }

  def filter(f: A => Boolean): Vec[A] = {
    val builder = Vec.newBuilder[A]
    foreach { a =>
      if(f(a))
        builder += a
    }
    builder.result()
  }

  def collect[@sp B: ClassTag](pf: PartialFunction[A, B]): Vec[B] = {
    val builder = Vec.newBuilder[B]
    foreach { a =>
      if(pf.isDefinedAt(a))
        builder += pf(a)
    }
    builder.result()
  }

  def forall(f: A => Boolean): Boolean = {
    cfor(0)(_ < length, _ + 1) { i =>
      if(!f(apply(i)))
        return false
    }
    true
  }
  def contains(a: A): Boolean = {
    cfor(0)(_ < length, _ + 1) { i =>
      if(a == apply(i))
        return true
    }
    false
  }
  def foldLeft[@specialized B](b: B)(f: (B, A) => B): B = {
    var cur = b
    foreach(a => cur = f(cur, a))
    cur
  }
  def distinct: Vec[A] = {
    if(length <= 1)
      return this
    val seen = debox.Set.empty[A]
    val builder = Vec.newBuilder[A]
    foreach(a => {
      if(!seen(a)) {
        seen += a
        builder += a
      }
    })
    builder.result()
  }

  def count(f: A => Boolean): Int = {
    var cnt = 0
    foreach(x => if(f(x)) cnt += 1)
    cnt
  }

  /**
    * Find the minimum value in this buffer.
    *
    * This method uses an instance of Spire's Order[A] type class to
    * compare the elements, to avoid boxing. If you want to use Scala's
    * Ordering, you can use compatibility code in Spire, or call
    * toIterable.min.
    */
  def min(implicit o: Order[A]): A = {
    if(isEmpty) throw new UnsupportedOperationException()
    var result: A = elems(0)
    val limit = length
    cfor(1)(_ < limit, _ + 1) { i =>
      result = result min elems(i)
    }
    result
  }

  /**
    * Find the maximum value in this buffer.
    *
    * This method uses an instance of Spire's Order[A] type class to
    * compare the elements, to avoid boxing. If you want to use Scala's
    * Ordering, you can use compatibility code in Spire, or call
    * toIterable.min.
    */
  def max(implicit o: Order[A]): A = {
    if(isEmpty) throw new UnsupportedOperationException()
    var result: A = elems(0)
    val limit = length
    cfor(1)(_ < limit, _ + 1) { i =>
      result = result max elems(i)
    }
    result
  }

  /**
    * Sort the contents of the buffer.
    *
    * This method uses an instance of Spire's Order[A] type class to
    * compare the elements, to avoid boxing. If you want to use Scala's
    * Ordering, you can use compatibility code in Spire, or call
    * toIterable.min.
    */
  def sorted(implicit o: Order[A]): Vec[A] = {
    val out = elems.clone()
    QuickSort.qsort(out, 0, length - 1)
    new Vec(out)
  }

  def isSorted(implicit o: Order[A]): Boolean = {
    cfor(1)(_ < length, _ + 1) { i =>
      if(o.gt(this(i - 1), this(i)))
        return false
    }
    true
  }

  def sortedBy[B](f: A => B)(implicit order: Order[B]): Vec[A] =
    sorted((x: A, y: A) => order.compare(f(x), f(y)))

  def zip[B](o: Vec[B]): Vec[(A, B)] = {
    if(o.size != size)
      throw new IllegalArgumentException("Arrays of different size")
    val out = Array.ofDim[(A, B)](length)
    cfor(0)(_ < length, _ + 1) { i =>
      out(i) = (this(i), o(i))
    }
    Vec.unsafe(out)
  }

  /**
    * Create an array out of the elements in the buffer.
    *
    * This is an O(n) operation, where n is the length of the buffer.
    */
  def toArray[B >: A]: Array[B] = elems.clone().asInstanceOf[Array[B]]

  /**
    * Wrap this buffer in an Iterable[A] instance.
    *
    * This method exists as a cheap way to get compatibility with Scala
    * collections without copying/conversion. Note that since Scala
    * collections are not specialized, using this iterable will box
    * values as they are accessed (although the underlying array will
    * still be unboxed).
    *
    * Like iterator, this method directly wraps the buffer. Thus, you
    * should not mutate the buffer while using the resulting iterable,
    * or risk corruption and undefined behavior.
    *
    * To get a "safe" value that is compatible with Scala collections,
    * consider using toVector, toList, or copy.toIterable.
    *
    * Creating the Iterable[A] instance is an O(1) operation.
    */
  def toIterable: scala.collection.immutable.Iterable[A] =
    new scala.collection.immutable.Iterable[A] {
      override def size: Int = lhs.length
      def iterator: Iterator[A] = lhs.iterator
      override def foreach[U](f: A => U): Unit = lhs.foreach(a => f(a))
    }

  def toSet: Set[A] = toIterable.toSet
  def toSeq: Seq[A] = toIterable.toSeq

  /**
    * Create a Vector[A] from this buffer's elements.
    *
    * This is an O(n) operation.
    */
  def toVector: Vector[A] = {
    import scala.collection.immutable.VectorBuilder
    val b = new VectorBuilder[A]
    b.sizeHint(length)
    cfor(0)(_ < length, _ + 1) { i =>
      b += elems(i)
    }
    b.result
  }

  /**
    * Create a List[A] from this buffer's elements.
    *
    * This is an O(n) operation.
    */
  def toList: List[A] = {
    var l = List[A]()
    cfor(length - 1)(_ >= 0, _ - 1) { i =>
      l = elems(i) :: l
    }
    l
  }
}

object Vec {

  /**
    * Allocate an empty Buffer.
    */
  def empty[@sp A: ClassTag]: Vec[A] = unsafe(Array.empty[A])

  /**
    * Fill a length-n Buffer with a constant value.
    *
    * If A is a reference type, all the elements in the Buffer will
    * point to the same 'a' instance. If it is known to be a value type
    * (e.g. Int) then all the values will be primitives.
    */
  def fill[@sp A: ClassTag](n: Int)(a: A): Vec[A] =
    unsafe(Array.fill(n)(a))

  /**
    * Wrap an array instance directly in a Buffer.
    *
    * This method is named 'unsafe' because the underlying array could
    * potentially be modified somewhere else, changing or corrupting
    * the Buffer. You should only use this method when you know that
    * the array will not be stored or modified externally.
    */
  def unsafe[@sp A: ClassTag](arr: Array[A]): Vec[A] =
    new Vec(arr)

  /**
    * Build a Buffer instance from the provided values.
    */
  def apply[A: ClassTag](args: A*): Vec[A] = unsafe(args.toArray)

  /**
    * Build a Buffer from the provided array.
    *
    * Unlike 'unsafe' this method clones the given array, to prevent
    * possible corruption later.
    */
  def fromArray[@sp A: ClassTag](arr: Array[A]): Vec[A] = unsafe(arr.clone)

  def newBuilder[A](implicit elemTag: ClassTag[A]): mutable.Builder[A, Vec[A]] =
    mutable.ArrayBuilder.make[A]().mapResult(unsafe(_))

  /**
    * Build a Buffer from the provided iterable object.
    */
  def fromIterable[@sp A: ClassTag](items: Iterable[A]): Vec[A] =
    unsafe(items.toArray)

  def fromSeq[@sp A: ClassTag](items: Seq[A]): Vec[A] = fromIterable(items)

  /**
    * Provide an Order[Buffer[A]] instance.
    *
    * The empty buffer is considered "less-than" any non-empty buffer,
    * and non-empty buffers are compared lexicographically. Elemens are
    * compared using the given Order[A].
    */
  implicit def order[@sp A: Order]: Order[Vec[A]] =
    new Order[Vec[A]] {
      def compare(lhs: Vec[A], rhs: Vec[A]): Int = {
        val (minLength, lastResult) =
          if(lhs.length < rhs.length) (lhs.length, -1)
          else if(lhs.length == rhs.length) (lhs.length, 0)
          else (rhs.length, 1)

        cfor(0)(_ < minLength, _ + 1) { i =>
          val n = lhs.elems(i) compare rhs.elems(i)
          if(n != 0) return n
        }
        lastResult
      }
    }

  /********** Extractors ********/
  object Vec1 {
    def unapply[@specialized(Int) A](arg: Vec[A]): Option[A] =
      if(arg.length == 1) Some(arg(0))
      else None
  }

  object Vec2 {
    def unapply[@specialized(Int) A](arg: Vec[A]): Option[(A, A)] =
      if(arg.length == 2) Some((arg(0), arg(1)))
      else None
  }

  object Vec3 {
    def unapply[@specialized(Int) A](arg: Vec[A]): Option[(A, A, A)] =
      if(arg.length == 3) Some((arg(0), arg(1), arg(2)))
      else None
  }

  /******** Ops **********/
  implicit class VecOfPairOps[A, B](private val arr: Vec[(A, B)]) extends AnyVal {
    def toMap: Map[A, B] = {
      val builder = Map.newBuilder[A, B]
      arr.foreach(ab => builder += ab)
      builder.result()
    }
  }

  /******** Type classes *******/
  implicit object sTraverseInstance extends STraverse[Vec] {
    override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: Vec[A])(f: A => B): Vec[B] =
      fa.map(f)

    override def traverse[G[_]: SApplicative, A, B: ClassTag](fa: Vec[A])(
        f: A => G[B]): G[Vec[B]] = {
      fa.foldLeft(SApplicative[G].pure(Vec[B]()))((acc, a) =>
        SApplicative[G].map2(acc, f(a))(_ :+ _))
    }

  }
}
