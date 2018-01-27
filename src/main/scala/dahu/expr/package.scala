package dahu

package object expr {

  type Type       = scala.reflect.runtime.universe.Type
  type WTypeTag[x] = scala.reflect.runtime.universe.WeakTypeTag[x]
  def typeOf[T](implicit ttag: WTypeTag[T]): Type = ttag.tpe
}
