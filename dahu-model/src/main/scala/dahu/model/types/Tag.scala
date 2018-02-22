package dahu.model.types

import scala.annotation.switch

trait Tag[T] {

  def typ: Tag.Type

}

/** A type for which an isomophism to a subset of integers is known. */
trait TagIsoInt[T] extends Tag[T] {

  def fromInt(i: Int): T
  def toValue(i: Int): Value = Value(fromInt(i))
  def toInt(t: T): Int
  def toIntUnsafe(t: Any): Int = toInt(t.asInstanceOf[T])

  val min: Int
  val max: Int
  def numInstances: Int = max - min + 1
}

object TagIsoInt {

  def apply[T](implicit ev: TagIsoInt[T]): TagIsoInt[T] = ev

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

object Tag {
  import scala.reflect.runtime.universe
  type Type = universe.Type

  def typeOf[T](implicit ttag: universe.WeakTypeTag[T]): universe.Type = ttag.tpe

  implicit case object IntTag extends TagIsoInt[Int] {
    override val typ: Type = typeOf[Int]
    override def fromInt(i: Int): Int = i
    override def toInt(t: Int): Int = t

    // todo: put real bounds
    override val min: Int = -100 //Integer.MIN_VALUE /2 +1
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
  def tagInstance[T](implicit ttag: universe.WeakTypeTag[T]): Tag[T] = new Tag[T] {
    override def typ: Type = ttag.tpe
  }
  implicit val DoubleTag = tagInstance[Double]

  implicit def optionTag[T](implicit ev: universe.WeakTypeTag[Option[T]]): Tag[Option[T]] = new Tag[Option[T]] {
    override def typ: Type = ev.tpe
  }
  implicit def eitherTag[L,R](implicit ev: universe.WeakTypeTag[Either[L,R]]): Tag[Either[L,R]] = new Tag[Either[L,R]] {
    override def typ: Type = ev.tpe
  }

//  implicit def default[T: universe.WeakTypeTag]: Tag[T] = new Tag[T] {
//    override def typ: Type = typeOf[T]
//  }
}
