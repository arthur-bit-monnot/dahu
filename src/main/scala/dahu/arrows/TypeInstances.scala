package dahu.arrows

import scalaz.Memo

//import dahu.arrows.memoization.Memo.Memo

trait TypeInstances[@specialized(Int) T] {

  /** All instances of T in no particular order. */
  def enumerate: Array[T]

  type Col[x] <: Memo[T, x]
}

trait OpaqueIntSubset[@specialized(Int) T] extends TypeInstances[T] {
  def first: T
  def last: T

  def unwrap(a: T): Int
  def wrap(i: Int): T

  def subst[F[_]](fi: F[Int]): F[T]
  def unsubst[F[_]](fa: F[T]): F[Int]
}
