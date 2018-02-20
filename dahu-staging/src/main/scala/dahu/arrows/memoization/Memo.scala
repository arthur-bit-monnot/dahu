package dahu.maps.memoization

import dahu.maps.{==>, OpaqueIntSubset, TypeInstances}

import scala.reflect.ClassTag

object Memo {

  trait Memo[A, B] {
    def apply(a: A): B
  }

  def memoize[A, B](values: Set[A], f: A => B): Memo[A, B] =
    new Memo[A, B] {
      val m: Map[A, B]            = values.map(x => (x, f(x))).toMap
      override def apply(a: A): B = m(a)
    }

  def memoize[A, B](f: A => B)(implicit instances: TypeInstances[A]): Memo[A, B] = new Memo[A, B] {
    val m: Map[A, B]            = instances.enumerate.map(x => (x, f(x))).toMap
    override def apply(a: A): B = m(a)
  }

  class ArrayMemo[A, B](f: A ==> B)(implicit ev: OpaqueIntSubset[A], tag: ClassTag[B])
      extends Memo[A, B] {
    import ev._
    private val offset: Int = unwrap(ev.first)

    private val inputs: Array[A] = ev.enumerate
    val memory: Array[B]         = new Array[B](1 + unwrap(last) - unwrap(first));
    {
      var i = 0
      while(i < inputs.length) {
        memory(unwrap(inputs(i)) - offset) = f(inputs(i))
        i += 1
      }
    }

    override def apply(a: A): B = memory(unwrap(a) - offset)
  }
}
