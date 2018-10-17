package dahu.graphs.transformations

trait TransformationWithSubstitution[F[_], G[_]] {
  def transformation[I <: Int](retrieve: I => G[I]): F[I] => (G[I], Option[I => I])
}
