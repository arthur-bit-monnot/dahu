package dahu.expr

import algebra.Order
import algebra.ring.{AdditiveCommutativeMonoid, Field, MultiplicativeCommutativeMonoid}
import spire.algebra.{AdditiveSemigroup, CRing, MultiplicativeSemigroup}

object Math extends App {
  import algebra.instances.all._

  val x = implicitly[Field[Double]]
  val y = implicitly[AdditiveCommutativeMonoid[Double]]
  val z = implicitly[MultiplicativeCommutativeMonoid[Double]]

  println(x)
  println(y)
  println(z)

  println("test")
  import scala.reflect.runtime.universe.TypeTag

  def addition2[T: AdditiveSemigroup: TypeTag] = new Fun2[T, T, T] {
    val additiveSemigroup = implicitly[AdditiveSemigroup[T]]

    override def of(in1: T, in2: T): T = additiveSemigroup.plus(in1, in2)

    override def name: String = "add"
  }

  def multiplication2[T: MultiplicativeSemigroup: TypeTag] = new Fun2[T, T, T] {
    val mulSG = implicitly[MultiplicativeSemigroup[T]]

    override def of(in1: T, in2: T): T = MultiplicativeSemigroup[T].times(in1, in2)

    override def name: String = "mul"
  }

  def leq2[T: Order: TypeTag] = new Fun2[T, T, Boolean] {
    val order = implicitly[Order[T]]

    override def of(in1: T, in2: T): Boolean = order.lteqv(in1, in2)

    override def name: String = "leq"
  }
}
