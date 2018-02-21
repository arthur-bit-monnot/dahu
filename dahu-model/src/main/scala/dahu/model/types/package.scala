package dahu.model

import algebra.Order

import scala.reflect.ClassTag

package object types {

  type Type = types.Tag[_]
  type WTypeTag[x] = types.Tag[x]
  def typeOf[T](implicit ttag: WTypeTag[T]): Type = ttag

  protected sealed abstract class ValueLabelImpl {
    type T
    def apply(s: Any): T
    def unwrap(lbl: T): Any
  }

  // do not forget `: LabelImpl`; it is key
  val Value: ValueLabelImpl = new ValueLabelImpl {
    type T = Any
    override def apply(s: Any) = s
    override def unwrap(lbl: T) = lbl
  }

  type Value = Value.T

  implicit val valueClassTag: ClassTag[Value] = ClassTag.Any.asInstanceOf[ClassTag[Value]]

}
