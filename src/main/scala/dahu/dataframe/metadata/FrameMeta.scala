package dahu.dataframe.metadata

import shapeless._
import shapeless.ops.nat.ToInt

import scala.annotation.implicitNotFound

sealed trait ColType[T]
trait Raw[T] extends ColType[T]
trait Indexed[T] extends ColType[T]

trait ColMeta {
  type K
  type V
}
object ColMeta {
  type KAux[K0] = ColMeta { type K = K0 }
  type KVAux[K0, V0] = ColMeta { type K = K0; type V = V0 }

  implicit def default[K0, V0]: ColMeta.KVAux[K0, V0] = new ColMeta {
    override type V = V0
    override type K = K0
  }
}

sealed trait FrameMeta {
  type Keys <: HList
  type Fields <: HList
  type Size <: Nat

  def :::[T <: ColMeta](prev: T) = dahu.dataframe.metadata.:::(prev, this)
}
object FrameMeta {
  type SizeAux[N <: Nat] = FrameMeta { type Size = N }
}
sealed trait EmptyFrame extends FrameMeta {
  override type Keys = HNil
  override type Fields = HNil
  override type Size = shapeless._0

  override def :::[T <: ColMeta](prev: T) = dahu.dataframe.metadata.:::(prev, this)
}
case object EmptyFrame extends EmptyFrame

final case class :::[H <: ColMeta, T <: FrameMeta](head: H, tail: T)
    extends FrameMeta {
  override type Keys = head.K :: tail.Keys
  override type Fields = head.V :: tail.Fields
  override type Size = Succ[tail.Size]

  override def :::[T <: ColMeta](prev: T) = dahu.dataframe.metadata.:::(prev, this)
}

@implicitNotFound(msg = "Could not find key `${K}` in frame metadata `${M}`")
trait ColumnMeta[K, M <: FrameMeta] {
  type Out <: ColMeta
}
object ColumnMeta {
  type Aux[K, M <: FrameMeta, V <: ColMeta] = ColumnMeta[K, M] { type Out = V }

  def apply[K, M <: FrameMeta](implicit ev: ColumnMeta[K, M]) = ev

  implicit def columnTypeOfHead[K, H <: ColMeta, T <: FrameMeta](
      implicit ev: H <:< ColMeta.KAux[K]): ColumnMeta.Aux[K, H ::: T, H] = {
    new ColumnMeta[K, H ::: T] {
      override type Out = H
    }
  }
  implicit def columnTypeOfOthers[K,
                                  H <: ColMeta,
                                  T <: FrameMeta,
                                  V <: ColMeta](
      implicit ev: ColumnMeta.Aux[K, T, V],
      ev2: H <:!< ColMeta.KAux[K]): ColumnMeta.Aux[K, H ::: T, V] = {
    new ColumnMeta[K, H ::: T] {
      override type Out = V
    }
  }
}

trait IndexOf[K, M <: FrameMeta] {
  type Out <: Nat
  def apply(): Int
}
object IndexOf {
  type Aux[K, M <: FrameMeta, N <: Nat] = IndexOf[K, M] { type Out = N }

  def apply[K, M <: FrameMeta](implicit instance: IndexOf[K, M]) = instance

  implicit def indexOfHead[K, N <: Nat, H <: ColMeta.KAux[K], T <: FrameMeta](
      implicit ev1: T <:< FrameMeta.SizeAux[N],
      ev: ToInt[N]
  ): IndexOf.Aux[K, H ::: T, N] = {
    new IndexOf[K, H ::: T] {
      override type Out = N
      override def apply(): Int = ev()
    }
  }
  implicit def indexOfOthers[K, H <: ColMeta, T <: FrameMeta, N <: Nat](
      implicit ev: IndexOf.Aux[K, T, N],
      toInt: ToInt[N],
      ev2: H <:!< ColMeta.KAux[K]): IndexOf.Aux[K, H ::: T, N] = {
    new IndexOf[K, H ::: T] {
      override type Out = N
      override def apply(): Int = toInt()
    }
  }
}
