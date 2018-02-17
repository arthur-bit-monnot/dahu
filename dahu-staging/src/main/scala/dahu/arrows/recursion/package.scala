package dahu.arrows

import cats.Functor
import dahu.arrows.memoization.Cache

package object recursion {

  type Algebra[F[_], X]   = F[X] ==> X
  type Coalgebra[F[_], X] = X ==> F[X]

  def hylo[A, B, F[_]](coalgebra: Coalgebra[F, A], algebra: Algebra[F, B])(
      implicit F: Functor[F]): A ==> B = {
    def go(id: A): B =
      algebra(F.map(coalgebra(id))(go))
    Arrow.lift(go)
  }

  def memoizedHylo[A, B, F[_]](coalgebra: Coalgebra[F, A],
                               algebra: Algebra[F, B],
                               cache: Cache[A, B])(implicit F: Functor[F]): A ==> B = {
    def go(id: A): B =
      cache.getOrElseUpdate(id, algebra(F.map(coalgebra(id))(go)))
    Arrow.lift(go)
  }

}
