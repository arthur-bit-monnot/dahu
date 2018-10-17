package dahu.model.types

import dahu.model.functions.{Box, Unbox}
import dahu.model.input.{Cst, Expr}
import dahu.model.math.bool
import dahu.utils.ClassTag

import scala.annotation.switch

/** A type for which an isomophism to a subset of integers is known. */
trait TagIsoInt[T] extends Tag[T] { self =>

  def fromInt(i: Int): T
  def toValue(i: Int): Value = Value(fromInt(i))
  def toInt(t: T): Int
  def toIntUnsafe(t: Any): Int = toInt(t.asInstanceOf[T])

  val min: Int
  val max: Int
  def numInstances: Int = max - min + 1

  val rawType: RawInt with Tag[Int] = new RawInt with Tag[Int] {
    override def clazz: ClassTag[Int] = scala.reflect.ClassTag.Int
    override def min: Int = self.min
    override def max: Int = self.max
  }

  private implicit def selfTag: TagIsoInt[T] = this

  lazy val unbox: Unbox[T] = new Unbox[T]()(this)
  def box: Box[T] = unbox.reverse
}

trait RawInt extends TagAny {
  def min: Int
  def max: Int
  override def typ: Tag.Type = Tag.typeOf[Int]

}

object TagIsoInt {

  def apply[T](implicit ev: TagIsoInt[T]): TagIsoInt[T] = ev

  implicit case object ofInt extends RawInt with Tag[Int] {
    override def clazz: ClassTag[Int] = scala.reflect.ClassTag.Int
    override val typ: Tag.Type = Tag.typeOf[Int]

    override val min: Int = Integer.MIN_VALUE / 2 + 1
    override val max: Int = Integer.MAX_VALUE / 2 - 1
  }
  import scala.reflect.runtime.universe
  def fromEnum[T: universe.WeakTypeTag: ClassTag](values: Seq[T]): TagIsoInt[T] = new TagIsoInt[T] {
    override def clazz: ClassTag[T] = implicitly[ClassTag[T]]
    override def toInt(t: T): Int = values.indexOf(t)
    override def fromInt(i: Int): T = values(i)

    override val min: Int = 0
    override val max: Int = values.size - 1

    override def typ: Tag.Type = Tag.typeOf[T]

    assert(numInstances == max - min + 1)
  }
}

/** Marker trait for a type that is a simple container of Int. */
trait BoxedInt[T] extends TagIsoInt[T] {}
