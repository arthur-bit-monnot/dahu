package dahu.dataframe.metadata

import dahu.dataframe.vector.Vec
import shapeless._
import shapeless.ops.nat.ToInt

import scala.annotation.implicitNotFound

sealed trait ColType[T]
trait Raw[T] extends ColType[T]
trait Indexed[T] extends ColType[T]

trait ColMeta {
  type K
  type V
  type F[_]

  val vec: Vec[F, V]
}
object ColMeta {
  type KAux[K0] = ColMeta { type K = K0 }
  type KVAux[K0, V0] = ColMeta { type K = K0; type V = V0 }
  type Aux[K0, V0, F0[_]] = ColMeta { type K = K0; type V = V0; type F[T] = F0[T] }

  implicit def default[K0, V0]: ColMeta.KVAux[K0, V0] = new ColMeta {
    override type V = V0
    override type K = K0
    override type F[T] = Vector[T]
    override val vec: Vec[Vector, V] = Vec.ofVector[V]
  }
}

sealed trait FrameMeta {
  type Keys <: HList
  type Fields <: HList
  type Size <: Nat

  def :::[T <: ColMeta](prev: T): T ::: this.type = dahu.dataframe.metadata.:::(prev, this)
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
  def apply(l: M): Out
}
object ColumnMeta {
  type Aux[K, M <: FrameMeta, V <: ColMeta] = ColumnMeta[K, M] { type Out = V }

  def apply[K, M <: FrameMeta](implicit ev: ColumnMeta[K, M]) = ev

  implicit def columnTypeOfHead[K, H <: ColMeta, T <: FrameMeta](
      implicit ev: H <:< ColMeta.KAux[K]): ColumnMeta.Aux[K, H ::: T, H] = {
    new ColumnMeta[K, H ::: T] {
      override type Out = H

      override def apply(l: H ::: T): H = l.head
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

      override def apply(l: H ::: T): V = ev.apply(l.tail)
    }
  }
}
