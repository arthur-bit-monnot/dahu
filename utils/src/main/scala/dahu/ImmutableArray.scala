package dahu

import scala.{specialized => sp}
import spire.syntax.cfor

import reflect.ClassTag
import collection.immutable.IndexedSeq
import collection.mutable.{ArrayBuilder, Builder}
import collection.IndexedSeqOptimized

import scala.language.implicitConversions

/**
  * An immutable wrapper for arrays
  *
  * @tparam A type of the elements of the array
  */
sealed trait ImmutableArray[@sp A] {
  protected[this] def elemTag: ClassTag[A]

  def apply(index: Int): A

  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit

  def length: Int

  def isEmpty: Boolean = length == 0

  def toArray[B >: A : ClassTag]: Array[B]

  def slice(from: Int, until: Int): ImmutableArray[A]

  def ++[B >: A: ClassTag](other: ImmutableArray[B]): ImmutableArray[B]
}

sealed abstract class ImmutableArrayInstances {

  implicit object sFunctorInstance extends SFunctor[ImmutableArray] {
    override def smap[@sp(Int) A, @sp(Int) B](fa: ImmutableArray[A])(f: A => B)(implicit ct: ClassTag[B]): ImmutableArray[B] = {
      val fb = Array.ofDim[B](fa.length)
      cfor.cfor(0)(_ < fa.length, _ + 1) { i =>
        fb(i) = f(fa(i))
      }
      ImmutableArray.fromArray(fb)
    }
  }

}

object ImmutableArray extends ImmutableArrayInstances {

  def make[A](x: AnyRef): ImmutableArray[A] = {
    val y = x match {
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
      case x: Array[AnyRef]  => new ofRef(x)
    }
    y.asInstanceOf[ImmutableArray[A]]
  }

  /**
    * Wrap `x` in an `ImmutableArray`.
    *
    * Provides better type inference than `make[A]`
    */
  def fromArray[A](x: Array[A]): ImmutableArray[A] = {
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
    y.asInstanceOf[ImmutableArray[A]]
  }

  def newBuilder[A](implicit elemTag: ClassTag[A]): Builder[A, ImmutableArray[A]] =
    ArrayBuilder.make[A]()(elemTag).mapResult(make(_))

  sealed trait ImmutableArray1[@sp A] extends ImmutableArray[A] {
    protected val array: Array[A]
    protected def upcast[B >: A]: Array[B] = array.asInstanceOf[Array[B]]
    def componentType: Class[_] = array.getClass().getComponentType

    def apply(idx: Int): A = array(idx)

    def length: Int = array.length
    def toArray[B >: A : ClassTag]: Array[B] = array.clone.asInstanceOf[Array[B]]
    def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = { array.copyToArray(xs, start, len) }

    def slice(from: Int, until: Int): ImmutableArray[A] = fromArray(array.slice(from, until))

    // TODO can do O(1) for primitives
    override def ++[B >: A: ClassTag](other: ImmutableArray[B]) = {
      val newArr = new Array(length + other.length)
      this.copyToArray(newArr, 0, length)
      other.copyToArray(newArr, length, other.length)
      fromArray(newArr)
    }

    override def hashCode(): Int = ??? // left non implemented
    override def equals(o: scala.Any): Boolean = ???
  }

  final class ofRef[A <: AnyRef](protected val array: Array[A]) extends ImmutableArray1[A] {
    protected[this] lazy val elemTag = ClassTag[A](componentType)

    override def hashCode(): Int = java.util.Arrays.deepHashCode(array.asInstanceOf[Array[AnyRef]])

    override def equals(o: scala.Any): Boolean = o match {
      case x: ofRef[_] => java.util.Arrays.equals(upcast[AnyRef], x.upcast[AnyRef])
      case _ => false
    }
  }

  final class ofByte(protected val array: Array[Byte]) extends ImmutableArray1[Byte] {
    protected[this] def elemTag = ClassTag.Byte
  }

  final class ofShort(protected val array: Array[Short]) extends ImmutableArray1[Short] {
    protected[this] def elemTag = ClassTag.Short
  }

  final class ofChar(protected val array: Array[Char]) extends ImmutableArray1[Char] {
    protected[this] def elemTag = ClassTag.Char

    // def mkString = new String(arr)
    // TODO why can elemTag be protected, but arr can't?
  }

  final class ofInt(protected val array: Array[Int]) extends ImmutableArray1[Int] {
    protected[this] def elemTag = ClassTag.Int

    override def hashCode(): Int = java.util.Arrays.hashCode(array)

    override def equals(o: scala.Any): Boolean = o match {
      case x: ofInt => java.util.Arrays.equals(array, x.array)
      case _ => false
    }
  }

  final class ofLong(protected val array: Array[Long]) extends ImmutableArray1[Long] {
    protected[this] def elemTag = ClassTag.Long
  }

  final class ofFloat(protected val array: Array[Float]) extends ImmutableArray1[Float] {
    protected[this] def elemTag = ClassTag.Float
  }

  final class ofDouble(protected val array: Array[Double]) extends ImmutableArray1[Double] {
    protected[this] def elemTag = ClassTag.Double
  }

  final class ofBoolean(protected val array: Array[Boolean]) extends ImmutableArray1[Boolean] {
    protected[this] def elemTag = ClassTag.Boolean
  }

  final class ofUnit(protected val array: Array[Unit]) extends ImmutableArray1[Unit] {
    protected[this] def elemTag = ClassTag.Unit
  }

  /********** Extractors ********/

  object Arr1 {
    def unapply[@specialized(Int) A](arg: ImmutableArray[A]): Option[A] =
      if(arg.length == 1) Some(arg(0))
      else None
  }

  object Arr2 {
    def unapply[@specialized(Int) A](arg: ImmutableArray[A]): Option[(A, A)] =
      if(arg.length == 2) Some((arg(0), arg(1)))
      else None
  }

  object Arr3 {
    def unapply[@specialized(Int) A](arg: ImmutableArray[A]): Option[(A, A, A)] =
      if(arg.length == 2) Some((arg(0), arg(1), arg(2)))
      else None
  }



  /********* Conversion as scala Seq *********/
  // todo: remove as it has hidden costs


  implicit def wrapArray[A](immArray: ImmutableArray[A]): WrappedImmutableArray[A] = {
    import ImmutableArray.{WrappedImmutableArray => IAO}
    immArray match {
      case a: ofRef[_] => new IAO.ofRef(a).asInstanceOf[WrappedImmutableArray[A]]
      case a: ofByte => new IAO.ofByte(a)
      case a: ofShort => new IAO.ofShort(a)
      case a: ofChar => new IAO.ofChar(a)
      case a: ofInt => new IAO.ofInt(a)
      case a: ofLong => new IAO.ofLong(a)
      case a: ofFloat => new IAO.ofFloat(a)
      case a: ofDouble => new IAO.ofDouble(a)
      case a: ofBoolean => new IAO.ofBoolean(a)
      case a: ofUnit => new IAO.ofUnit(a)
    }
  }

  implicit def unwrapArray[A](immArrayOps: WrappedImmutableArray[A]): ImmutableArray[A] = immArrayOps.value

  abstract class WrappedImmutableArray[A](val value: ImmutableArray[A]) extends
          IndexedSeq[A] with IndexedSeqOptimized[A, WrappedImmutableArray[A]] {
    def apply(index: Int) = value(index)
    def length = value.length

    override def stringPrefix = "ImmutableArray"

    protected[this] def arrayBuilder: Builder[A, ImmutableArray[A]]

    override protected[this] def newBuilder: Builder[A, WrappedImmutableArray[A]] = arrayBuilder.mapResult(wrapArray)
  }

  object WrappedImmutableArray {
    import dahu.{ImmutableArray => IA}

    abstract class ofImmutableArray1[A](val immArray: ImmutableArray1[A]) extends WrappedImmutableArray[A](immArray) {
      protected[this] def elemTag: ClassTag[A]

      override protected[this] def arrayBuilder = ImmutableArray.newBuilder[A](elemTag)
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
