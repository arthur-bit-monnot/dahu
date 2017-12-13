package dahu.dataframe.metadata

import dahu.dataframe.vector.Vec
import shapeless._
import shapeless.ops.nat.ToInt

import scala.annotation.implicitNotFound

sealed trait ColType[T]
trait Raw[T] extends ColType[T]
trait Indexed[T] extends ColType[T]

trait ColMeta[K] {
  type V
  type F[_]

  val vec: Vec[F, V]
}
object ColMeta {
  type Aux[K, V0, F0[_]] = ColMeta[K] { type V = V0; type F[T] = F0[T] }

  implicit def default[K, V0]: ColMeta.Aux[K, V0, Vector] = new ColMeta[K] {
    override type V = V0
    override type F[T] = Vector[T]
    override val vec: Vec[Vector, V] = Vec.ofVector[V]
  }
}

sealed trait FrameMeta {
  type Fields <: HList
  type Size <: Nat

  def size: Int
}
object FrameMeta {
  type SizeAux[N <: Nat] = FrameMeta { type Size = N }

  implicit class FrameMetaOps[A <: FrameMeta](val a: A) extends AnyVal {
    def :::[B <: ColMeta[_]](b: B): B ::: A =
      dahu.dataframe.metadata.:::[B, A](b, a)
  }
}
sealed trait EmptyFrame extends FrameMeta {
  override type Fields = HNil
  override type Size = shapeless._0

  override def size: Int = 0
}
case object EmptyFrame extends EmptyFrame

final case class :::[H <: ColMeta[_], T <: FrameMeta](head: H, tail: T)
    extends FrameMeta {
  override type Fields = head.V :: tail.Fields
  override type Size = Succ[tail.Size]

  override def size: Int = tail.size + 1
}

@implicitNotFound(msg = "Could not find key `${K}` in frame metadata `${M}`")
trait ColumnMeta[K, M <: HList] {
  type Out
  def apply(l: M): Out
}

trait LowPriorityColumnMeta {}

object ColumnMeta {
  type Aux[K, M <: HList, Out0] = ColumnMeta[K, M] { type Out = Out0 }

  def apply[K, M <: HList](implicit ev: ColumnMeta[K, M]): Aux[K, M, ev.Out] = ev

  implicit def columnTypeOfHead[K, H, T <: HList](
      implicit key: Key.Aux[H, K]): ColumnMeta.Aux[K, H :: T, H] = {
    new ColumnMeta[K, H :: T] {
      override type Out = H

      override def apply(l: H :: T): H = l.head
    }
  }

  implicit def columnTypeOfOthers[K, H, T <: HList, Out0](
      implicit ev: ColumnMeta.Aux[K, T, Out0]): ColumnMeta.Aux[K, H :: T, Out0] = {
    new ColumnMeta[K, H :: T] {
      override type Out = Out0

      override def apply(l: H :: T): Out = ev.apply(l.tail)
    }
  }
}
