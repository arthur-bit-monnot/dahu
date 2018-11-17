package dahu.model.problem.syntax

import cats.kernel.Order
import cats.implicits._
import dahu.model.ir.ComputationF
import dahu.model.math._
import dahu.graphs._
import dahu.model.types.Tag
import dahu.utils._

object Not {
  def apply[@sp(Int) A: ClassTag](a: A): ComputationF[A] =
    ComputationF(bool.Not, Vec(a), Tag.ofBoolean)
  def unapply[A](c: ComputationF[A]): Option[A] = c match {
    case ComputationF(bool.Not, Vec(a), _) => Some(a)
    case _                                 => None
  }
}

object Or {
  def apply[A <: Int](cs: Iterable[A]): ComputationF[A] =
    ComputationF(bool.Or, Vec.fromIterable(cs), Tag.ofBoolean)
  def apply[A: ClassTag](a: A, b: A): ComputationF[A] =
    ComputationF(bool.Or, Vec(a, b), Tag.ofBoolean)

  def unapplySeq[A](c: ComputationF[A]): Option[Seq[A]] = c match {
    case ComputationF(bool.Or, args, _) => Some(args.toSeq)
    case _                              => None
  }
}
object And {
  implicit def intOrder[I <: Int]: Order[I] = Order[Int].asInstanceOf[Order[I]]
  def apply[A <: Int](a: A, b: A): ComputationF[A] =
    ComputationF(bool.And, Vec(a, b), Tag.ofBoolean)
  def apply[A <: Int](cs: Iterable[A]): ComputationF[A] =
    ComputationF(bool.And, Vec.fromIterable(cs).sorted, Tag.ofBoolean)
  def unapplySeq[A](c: ComputationF[A]): Option[Seq[A]] = c match {
    case ComputationF(bool.And, args, _) => Some(args.toSeq)
    case _                               => None
  }
}
