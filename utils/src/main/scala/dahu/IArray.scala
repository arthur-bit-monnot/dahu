package dahu

import cats.Order

import scala.{specialized => sp}
import spire.syntax.cfor._

import reflect.ClassTag
import collection.mutable.{ArrayBuilder, Builder}
import collection.{mutable, IndexedSeqOptimized}
import scala.language.implicitConversions

/**
  * An immutable wrapper for arrays
  *
  * @tparam A type of the elements of the array
  */
sealed trait IArray[@sp A] {
  protected[this] def elemTag: ClassTag[A]

  def apply(index: Int): A

  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit

  def upcast[B >: A]: IArray[B] = this.asInstanceOf[IArray[B]]

  def length: Int
  @inline def size: Int = length

  def indices: Range = 0 until length

  def isEmpty: Boolean = length == 0

  def toArray[B >: A: ClassTag]: Array[B]

  def slice(from: Int, until: Int): IArray[A]

  def ++[B >: A: ClassTag](other: IArray[B]): IArray[B]

  def mkString(sep: String, start: String = "", end: String = ""): String = {
    val sb = new mutable.StringBuilder()
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

  def toList: List[A] = {
    var l = List[A]()
    cfor(size - 1)(_ >= 0, _ - 1) { i =>
      l = apply(i) :: l
    }
    l
  }
  def toSeq: Seq[A] = IArray.wrapArray(this)
  def toSet: Set[A] = {
    val builder = Set.newBuilder[A]
    foreach(a => builder += a)
    builder.result()
  }

  @inline def foreach(f: A => Unit): Unit = cfor(0)(_ < length, _ + 1) { i =>
    f(apply(i))
  }
  def filter(f: A => Boolean): IArray[A] = {
    val builder = IArray.newBuilder[A](elemTag)
    foreach { a =>
      if(f(a))
        builder += a
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

  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    var cur = b
    foreach(a => cur = f(cur, a))
    cur
  }
  def map[@specialized(Int) B: ClassTag](f: A => B): IArray[B] = {
    val builder = IArray.newBuilder[B]
    builder.sizeHint(length)
    foreach(a => builder += f(a))
    builder.result()
  }
  def distinct: IArray[A] = {
    if(size == 1)
      return this
    val seen = debox.Set.empty(elemTag)
    val builder = IArray.newBuilder[A](elemTag)
    foreach(a => {
      if(!seen(a)) {
        seen += a
        builder += a
      }
    })
    builder.result()
  }
  def sorted(implicit order: Order[A]): IArray[A] = {
    val mut = debox.Buffer.fromArray(this.toArray(elemTag))(elemTag)
    mut.sort
    IArray.fromArray(mut.toArray())
  }
  def sortedBy[B](f: A => B)(implicit order: Order[B]): IArray[A] =
    sorted((x: A, y: A) => order.compare(f(x), f(y)))

  def zip[B](o: IArray[B]): IArray[(A, B)] = {
    if(o.size != size)
      throw new IllegalArgumentException("Arrays of different size")
    val builder = IArray.newBuilder[(A, B)]
    cfor(0)(_ < length, _ + 1) { i =>
      builder += ((this(i), o(i)))
    }
    builder.result()
  }
}

sealed abstract class ImmutableArrayInstances {

  implicit object sFunctorInstance extends SFunctor[IArray] {
    override def smap[@sp(Int) A, @sp(Int) B](fa: IArray[A])(f: A => B)(
        implicit ct: ClassTag[B]): IArray[B] = {
      val fb = Array.ofDim[B](fa.length)
      cfor(0)(_ < fa.length, _ + 1) { i =>
        fb(i) = f(fa(i))
      }
      IArray.fromArray(fb)
    }
  }

}

object IArray extends ImmutableArrayInstances {

  def apply[A: ClassTag](as: A*): IArray[A] = fromArray(as.toArray)
  def fromSeq[A: ClassTag](as: Seq[A]): IArray[A] = fromArray(as.toArray)

  def fill[A: ClassTag](n: Int)(value: => A): IArray[A] = fromArray(Array.fill(n)(value))

  /**
    * Wrap `x` in an immutable array IArray
    *
    * No copy of the array is made.
    */
  def fromArray[A](x: Array[A]): IArray[A] = {
    val y = x.asInstanceOf[AnyRef] match {
      case null              => null
      case x: Array[Byte]    => new ofByte(x)
      case x: Array[Short]   => new ofShort(x)
      case x: Array[Char]    => new ofChar(x)
      case x: Array[Int]     => new ofInt(x)
      case x: Array[Long]    => new ofLong(x)
      case x: Array[Float]   => new ofFloat(x)
      case x: Array[Double]  => new ofDouble(x)
      case x: Array[Boolean] => new ofBoolean(x)
      case x: Array[Unit]    => new ofUnit(x)
      case _: Array[AnyRef]  => new ofRef(x.asInstanceOf[Array[AnyRef]])
    }
    y.asInstanceOf[IArray[A]]
  }

  def newBuilder[A](implicit elemTag: ClassTag[A]): mutable.Builder[A, IArray[A]] =
    mutable.ArrayBuilder.make[A]()(elemTag).mapResult(fromArray(_))

  sealed trait IArray1[@sp A] extends IArray[A] {
    protected val array: Array[A]
    protected def covariant[B >: A]: Array[B] = array.asInstanceOf[Array[B]]
    def componentType: Class[_] = array.getClass().getComponentType

    def apply(idx: Int): A = array(idx)

    def length: Int = array.length
    def toArray[B >: A: ClassTag]: Array[B] = array.clone.asInstanceOf[Array[B]]
    def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
      array.copyToArray(xs, start, len)
    }

    def slice(from: Int, until: Int): IArray[A] = fromArray(array.slice(from, until))

    // TODO can do O(1) for primitives
    override def ++[B >: A: ClassTag](other: IArray[B]) = {
      val newArr = new Array(length + other.length)
      this.copyToArray(newArr, 0, length)
      other.copyToArray(newArr, length, other.length)
      fromArray(newArr)
    }

    override def hashCode(): Int = ??? // left non implemented
    override def equals(o: scala.Any): Boolean = ???
  }

  final class ofRef[A <: AnyRef](protected val array: Array[A]) extends IArray1[A] {
    protected[this] lazy val elemTag = ClassTag[A](componentType)

    override def hashCode(): Int = java.util.Arrays.deepHashCode(array.asInstanceOf[Array[AnyRef]])

    override def equals(o: scala.Any): Boolean = o match {
      case x: ofRef[_] => java.util.Arrays.equals(covariant[AnyRef], x.covariant[AnyRef])
      case _           => false
    }
  }

  final class ofByte(protected val array: Array[Byte]) extends IArray1[Byte] {
    protected[this] def elemTag = ClassTag.Byte
  }

  final class ofShort(protected val array: Array[Short]) extends IArray1[Short] {
    protected[this] def elemTag = ClassTag.Short
  }

  final class ofChar(protected val array: Array[Char]) extends IArray1[Char] {
    protected[this] def elemTag = ClassTag.Char

    // def mkString = new String(arr)
    // TODO why can elemTag be protected, but arr can't?
  }

  final class ofInt(protected val array: Array[Int]) extends IArray1[Int] {
    protected[this] def elemTag = ClassTag.Int

    override def hashCode(): Int = java.util.Arrays.hashCode(array)

    override def equals(o: scala.Any): Boolean = o match {
      case x: ofInt => java.util.Arrays.equals(array, x.array)
      case _        => false
    }
  }

  final class ofLong(protected val array: Array[Long]) extends IArray1[Long] {
    protected[this] def elemTag = ClassTag.Long
  }

  final class ofFloat(protected val array: Array[Float]) extends IArray1[Float] {
    protected[this] def elemTag = ClassTag.Float
  }

  final class ofDouble(protected val array: Array[Double]) extends IArray1[Double] {
    protected[this] def elemTag = ClassTag.Double
  }

  final class ofBoolean(protected val array: Array[Boolean]) extends IArray1[Boolean] {
    protected[this] def elemTag = ClassTag.Boolean
  }

  final class ofUnit(protected val array: Array[Unit]) extends IArray1[Unit] {
    protected[this] def elemTag = ClassTag.Unit
  }

  /********** Extractors ********/
  object Arr1 {
    def unapply[@specialized(Int) A](arg: IArray[A]): Option[A] =
      if(arg.length == 1) Some(arg(0))
      else None
  }

  object Arr2 {
    def unapply[@specialized(Int) A](arg: IArray[A]): Option[(A, A)] =
      if(arg.length == 2) Some((arg(0), arg(1)))
      else None
  }

  object Arr3 {
    def unapply[@specialized(Int) A](arg: IArray[A]): Option[(A, A, A)] =
      if(arg.length == 2) Some((arg(0), arg(1), arg(2)))
      else None
  }

  implicit class ArrayOfPairOps[A, B](private val arr: IArray[(A, B)]) extends AnyVal {
    def toMap: Map[A, B] = {
      val builder = Map.newBuilder[A, B]
      arr.foreach(ab => builder += ab)
      builder.result()
    }
  }

  /********* Conversion as scala Seq *********/
  // todo: remove as it has hidden costs

  def wrapArray[A](immArray: IArray[A]): WrappedImmutableArray[A] = {
    import IArray.{WrappedImmutableArray => IAO}
    immArray match {
      case a: ofRef[_]  => new IAO.ofRef(a).asInstanceOf[WrappedImmutableArray[A]]
      case a: ofByte    => new IAO.ofByte(a)
      case a: ofShort   => new IAO.ofShort(a)
      case a: ofChar    => new IAO.ofChar(a)
      case a: ofInt     => new IAO.ofInt(a)
      case a: ofLong    => new IAO.ofLong(a)
      case a: ofFloat   => new IAO.ofFloat(a)
      case a: ofDouble  => new IAO.ofDouble(a)
      case a: ofBoolean => new IAO.ofBoolean(a)
      case a: ofUnit    => new IAO.ofUnit(a)
    }
  }
//
//  implicit def unwrapArray[A](immArrayOps: WrappedImmutableArray[A]): ImmutableArray[A] = immArrayOps.value

  abstract class WrappedImmutableArray[A](val value: IArray[A])
      extends IndexedSeq[A]
      with IndexedSeqOptimized[A, WrappedImmutableArray[A]] {
    def apply(index: Int) = value(index)
    def length = value.length

    override def stringPrefix = "ImmutableArray"

    protected[this] def arrayBuilder: Builder[A, IArray[A]]

    override protected[this] def newBuilder: Builder[A, WrappedImmutableArray[A]] =
      arrayBuilder.mapResult(wrapArray)
  }

  object WrappedImmutableArray {
    import dahu.{IArray => IA}

    abstract class ofImmutableArray1[A](val immArray: IArray1[A])
        extends WrappedImmutableArray[A](immArray) {
      protected[this] def elemTag: ClassTag[A]

      override protected[this] def arrayBuilder = IArray.newBuilder[A](elemTag)
    }

    final class ofRef[A <: AnyRef](array: IA.ofRef[A]) extends ofImmutableArray1[A](array) {
      protected[this] lazy val elemTag = ClassTag[A](array.componentType)
    }

    final class ofByte(array: IA.ofByte) extends ofImmutableArray1[Byte](array) {
      protected[this] def elemTag = ClassTag.Byte
    }

    final class ofShort(array: IA.ofShort) extends ofImmutableArray1[Short](array) {
      protected[this] def elemTag = ClassTag.Short
    }

    final class ofChar(array: IA.ofChar) extends ofImmutableArray1[Char](array) {
      protected[this] def elemTag = ClassTag.Char
    }

    final class ofInt(array: IA.ofInt) extends ofImmutableArray1[Int](array) {
      protected[this] def elemTag = ClassTag.Int
    }

    final class ofLong(array: IA.ofLong) extends ofImmutableArray1[Long](array) {
      protected[this] def elemTag = ClassTag.Long
    }

    final class ofFloat(array: IA.ofFloat) extends ofImmutableArray1[Float](array) {
      protected[this] def elemTag = ClassTag.Float
    }

    final class ofDouble(array: IA.ofDouble) extends ofImmutableArray1[Double](array) {
      protected[this] def elemTag = ClassTag.Double
    }

    final class ofBoolean(array: IA.ofBoolean) extends ofImmutableArray1[Boolean](array) {
      protected[this] def elemTag = ClassTag.Boolean
    }

    final class ofUnit(array: IA.ofUnit) extends ofImmutableArray1[Unit](array) {
      protected[this] def elemTag = ClassTag.Unit
    }
  }

}
