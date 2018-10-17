package dahu.graphs.transformations

trait Transformation[F[_], G[_]] {
  def transformation[I <: Int](retrieve: I => G[I], record: G[I] => I): F[I] => G[I]
}
