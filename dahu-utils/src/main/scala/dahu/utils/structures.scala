package dahu.utils

import algebra.Order
import spire.syntax.cfor._

import scala.reflect.ClassTag

object structures {

  object ArrayUtils {
    def map[@specialized(Int) A, @specialized(Int) B](array: Array[A])(f: A => B)(
        implicit ct: ClassTag[B]): Array[B] = {
      val ret = Array.ofDim[B](array.length)
      var i = 0
      while(i < array.length) {
        ret(i) = f(array(i))
        i += 1
      }
      ret
    }

  }

  /** todo: check if we need to specialized this, e.g. requiring `A <: Int` to avoid boxing. */
  implicit final class BufferOps[A](val buff: debox.Buffer[A]) extends AnyVal {

    def indices: Range = 0 until buff.length

    def forall(f: A => Boolean): Boolean = {
      cforRange(0 until buff.length) { i =>
        if(!f(buff(i)))
          return false
      }
      true
    }

    def sortedBy[B: Order](f: A => B): debox.Buffer[A] = {
      val orderB = Order[B]
      buff.copy()
      val orderA = new Order[A] {
        override def compare(x: A, y: A): Int = orderB.compare(f(x), f(y))
      }
      buff.sort(orderA)
      buff
    }
  }

  implicit final class SetOps[A](val set: debox.Set[A]) extends AnyVal {

    def cast[B](implicit ctA: ClassTag[A], ctB: ClassTag[B]): debox.Set[B] = {
      assert(ctA == ctB)
      set.asInstanceOf[debox.Set[B]]
    }
  }

  /** Provides a default value for a type. */
  trait Default[A] {
    def apply(): A
  }
  object Default {
    def apply[A](implicit instance: Default[A]): Default[A] = instance
    def of[A](implicit instance: Default[A]): A = instance()

    implicit val defaultInt: Default[Int] = () => 0
    implicit val defaultBool: Default[Boolean] = () => false
    implicit val defaultAnyRef: Default[AnyRef] = () => null
    implicit val defaultAny: Default[Any] = () => null
    implicit def defaultRef[A <: AnyRef]: Default[A] = defaultAnyRef.asInstanceOf[Default[A]]
  }
}
