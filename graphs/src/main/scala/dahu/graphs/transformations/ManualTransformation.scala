package dahu.graphs.transformations

trait ManualTransformation[F[_], G[_]] {

  def trans[I <: Int, J <: Int](ctx: ManualTransformation.Context[F, G, I, J]): I => J
}

object ManualTransformation {

  trait Context[F[_], G[_], I <: Int, J <: Int] {
    def oget(i: I): F[I]
    def nget(j: J): G[J]
    def nrec(gj: G[J]): J
  }
}
