package dahu.expr.labels

import scala.reflect.ClassTag

object Labels {
  sealed abstract class ValueLabelImpl {
    type T
    def apply(s: Any): T
    def unwrap(lbl: T): Any
  }

  // do not forget `: LabelImpl`; it is key
  val Value: ValueLabelImpl = new ValueLabelImpl {
    type T = Any
    override def apply(s: Any)  = s
    override def unwrap(lbl: T) = lbl
  }

  type Value = Value.T

  implicit val classTag: ClassTag[Value] = ClassTag.Any.asInstanceOf[ClassTag[Value]]
}
