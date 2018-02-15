package dahu.utils

import algebra.Order
import spire.syntax.cfor._

object structures {

  /** todo: check if we need to specialized this, e.g. requiring `A <: Int` to avoid boxing. */
  implicit class BufferOps[A](val buff: debox.Buffer[A]) extends AnyVal {

    def indices: Range = 0 until buff.length

    def forall(f: A => Boolean): Boolean = {
      cforRange(0 until buff.length){ i =>
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

}
