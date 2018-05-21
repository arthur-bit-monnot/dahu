package dahu.model

package object functions {

  case class ->[I, O](f: I => O)

}
