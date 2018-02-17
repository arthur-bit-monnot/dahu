package dahu.dataframe.metadata

trait Key[C] {
  type Out
}
object Key {
  type Aux[C, K] = Key[C] { type Out = K }

  def apply[C](implicit ev: Key[C]): Aux[C, ev.Out] = ev
}

trait Value[C] {
  type Out
}
object Value {
  type Aux[C, V] = Value[C] { type Out = V }

  def apply[C](implicit ev: Value[C]): Aux[C, ev.Out] = ev
}

trait Container[C] {
  type Out[_]
}
object Container {
  type Aux[C, F[_]] = Container[C] { type Out[x] = F[x] }

  def apply[C](implicit ev: Container[C]): Aux[C, ev.Out] = ev
}

abstract class ColumMetadata[K, V, F[_]]

object ColumMetadata {

  implicit def key[K, V, F[_], C](implicit ev: C <:< ColumMetadata[K, V, F]): Key.Aux[C, K] =
    new Key[C] {
      override type Out = K
    }

  implicit def value[K, V, F[_], C](implicit ev: C <:< ColumMetadata[K, V, F]): Value.Aux[C, V] =
    new Value[C] { override type Out = V }

  implicit def column[K, V, F[_], C](
      implicit ev: C <:< ColumMetadata[K, V, F]): Container.Aux[C, F] =
    new Container[C] {
      override type Out[x] = F[x]
    }
}
