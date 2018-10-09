package dahu.model

import dahu.utils._
import shapeless.tag.@@

import scala.reflect.ClassTag

package object problem {

  trait Mark
  type IDTop = SInt[Mark]
  implicit val ct: ClassTag[IDTop] = ClassTag.Int.asInstanceOf[ClassTag[IDTop]]

  implicit def implicitClassTag[I <: IDTop]: ClassTag[I] = ct.asInstanceOf[ClassTag[I]]

  trait ExistentialMarker
  type SomeID = SubSubInt[IDTop, ExistentialMarker]

}
