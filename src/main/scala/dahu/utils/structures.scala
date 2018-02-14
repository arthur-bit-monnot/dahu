package dahu.utils

import algebra.Order

object structures {

  implicit class BufferOps[A](val buff: debox.Buffer[A]) extends AnyVal {

    def forall(f: A => Boolean): Boolean = {
      var i = 0
      while(i < buff.length) {
        if(!f(buff(i)))
          return false
        i += 1
      }
      true
    }

    def sortBy[B: Order](f: A => B): debox.Buffer[A] = {
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
