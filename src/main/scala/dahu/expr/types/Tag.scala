package dahu.expr.types

import scala.annotation.switch

trait Tag[T] {

  def typ: Tag.Type

}

/** A type for which an isomophism to a subset of integers is known. */
trait TagIsoInt[T] extends Tag[T] {


  def fromInt(i: Int): T
  def toInt(t: T) : Int
  def toIntUnsafe(t: Any): Int = toInt(t.asInstanceOf[T])

  val min: Int
  val max: Int
}

object Tag {
  import scala.reflect.runtime.universe
  type Type = universe.Type

  def typeOf[T](implicit ttag:universe.WeakTypeTag[T]): universe.Type = ttag.tpe

  implicit case object IntTag extends TagIsoInt[Int] {
    override val typ: Type = typeOf[Int]
    override def fromInt(i: Int): Int = i
    override def toInt(t: Int): Int = t

    override val min: Int = 0 //Integer.MIN_VALUE /2 +1
    override val max: Int = 100 //Integer.MAX_VALUE /2 -1
  }

  implicit case object BoolTag extends TagIsoInt[Boolean] {
    override def typ: Type = typeOf[Boolean]
    override def toInt(t: Boolean): Int = if(t) 1 else 0
    def fromInt(i: Int): Boolean = (i: @switch) match {
      case 0 => false
      case 1 => true
      case _ => ???
    }

    override val min: Int = 0
    override val max: Int = 1
  }

  implicit def default[T: universe.WeakTypeTag]: Tag[T] = new Tag[T] {
    override def typ: Type = typeOf[T]
  }
}