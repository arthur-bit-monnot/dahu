package dahu.model

import dahu.utils.structures.Default

import scala.reflect.ClassTag

package object types {

  type Bool <: Int

  type Type = types.TagAny
  def typeOf[T](implicit ttag: Tag[T]): Type = ttag

  implicit def classTag[A: Tag]: ClassTag[A] = Tag[A].clazz

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
  implicit val valueDefault: Default[Value] = Default[Any].asInstanceOf[Default[Value]]

}
