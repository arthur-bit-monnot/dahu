package dahu.arrows

import dahu.arrows.memoization.{ArrayCache, Cache}
import dahu.ast.IndexLabelImpl

import scala.reflect.ClassTag

trait TypeInstances[@specialized(Int) T] {

  /** All instances of T in no particular order. */
  def enumerate: Array[T]

}

trait OpaqueIntSubset[@specialized(Int) T] extends TypeInstances[T] {
  def first: T = wrap(unsubst(enumerate).min)
  def last: T  = wrap(unsubst(enumerate).max)

  def unwrap(a: T): Int
  def wrap(i: Int): T

  def subst[F[_]](fi: F[Int]): F[T]
  def unsubst[F[_]](fa: F[T]): F[Int]

  def newCache[B](implicit classTag: ClassTag[B]): Cache[T, B] = new ArrayCache[T, B](this)
}
