package dahu

package object expr {

  type Type       = scala.reflect.runtime.universe.Type
  type TypeTag[x] = scala.reflect.runtime.universe.TypeTag[x]
  def typeOf[T](implicit ttag: TypeTag[T]): Type = ttag.tpe
}
