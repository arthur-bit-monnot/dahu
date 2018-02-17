package dahu

package object expr {

  type Type        = types.Tag[_]
  type WTypeTag[x] = types.Tag[x]
  def typeOf[T](implicit ttag: WTypeTag[T]): Type = ttag
}
