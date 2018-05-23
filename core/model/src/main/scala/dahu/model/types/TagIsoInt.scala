package dahu.model.types

import dahu.model.functions.{Box, Unbox}

import scala.annotation.switch

/** A type for which an isomophism to a subset of integers is known. */
trait TagIsoInt[T] extends Tag[T] {

  def fromInt(i: Int): T
  def toValue(i: Int): Value = Value(fromInt(i))
  def toInt(t: T): Int
  def toIntUnsafe(t: Any): Int = toInt(t.asInstanceOf[T])

  val min: Int
  val max: Int
  def numInstances: Int = max - min + 1

  private implicit def selfTag: TagIsoInt[T] = this

  val unbox: Unbox[T] = new Unbox[T]()(this)
  val box: Box[T] = unbox.reverse
}

object TagIsoInt {

  def apply[T](implicit ev: TagIsoInt[T]): TagIsoInt[T] = ev

  implicit case object ofInt extends BoxedInt[Int] {
    override val typ: Tag.Type = Tag.typeOf[Int]
    override def fromInt(i: Int): Int = i
    override def toInt(t: Int): Int = t

    override val min: Int = Integer.MIN_VALUE / 2 + 1
    override val max: Int = Integer.MAX_VALUE / 2 - 1
  }

  implicit case object ofBoolean extends TagIsoInt[Boolean] {
    override def typ: Tag.Type = Tag.typeOf[Boolean]
    override def toInt(t: Boolean): Int = if(t) 1 else 0
    def fromInt(i: Int): Boolean = (i: @switch) match {
      case 0 => false
      case 1 => true
      case _ => ???
    }

    override val min: Int = 0
    override val max: Int = 1
  }

  import scala.reflect.runtime.universe
  def fromEnum[T: universe.WeakTypeTag](values: Seq[T]): TagIsoInt[T] = new TagIsoInt[T] {
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
