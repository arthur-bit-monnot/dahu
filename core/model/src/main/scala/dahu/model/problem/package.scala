package dahu.model

import dahu.utils._
import shapeless.tag.@@

import scala.reflect.ClassTag

package object problem {

  trait Mark
  type ID = SInt[Mark]
  implicit val ct: ClassTag[ID] = ClassTag.Int.asInstanceOf[ClassTag[ID]]

}
