package dahu

import dahu.utils._

import scala.reflect.ClassTag

package object graphs {

  trait Mark
  type IDTop = SInt[Mark]
  implicit val ct: ClassTag[IDTop] = ClassTag.Int.asInstanceOf[ClassTag[IDTop]]

  implicit def implicitClassTag[I <: IDTop]: ClassTag[I] = ct.asInstanceOf[ClassTag[I]]

  sealed trait ExistentialMarker
  type SomeID = SubSubInt[IDTop, ExistentialMarker]

}
