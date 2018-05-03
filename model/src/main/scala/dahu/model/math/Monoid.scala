package dahu.model.math

import dahu.model.functions.{Fun2, FunN}
import dahu.model.ir.CstF
import dahu.model.types._

trait Monoid[T] extends FunN[T, T] {
  def tpe: Tag[T]

  def combine(lhs: T, rhs: T): T
  def combineUnsafe(lhs: Value, rhs: Value): Value =
    Value(combine(lhs.asInstanceOf[T], rhs.asInstanceOf[T]))
  val identity: T
  final lazy val liftedIdentity: CstF[T] = CstF(Value(identity), tpe)

  override def of(args: Seq[T]): T = args.fold(identity)(combine)

}

trait CommutativeMonoid[T] extends Monoid[T]

trait IdempotentMonoid[T] extends Monoid[T]
