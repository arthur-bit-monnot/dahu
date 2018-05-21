package dahu.model

package object functions {

  final case class ->[I, O](f: I => O)

}
