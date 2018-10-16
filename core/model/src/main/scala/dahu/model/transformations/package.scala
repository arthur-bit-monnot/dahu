package dahu.model
import dahu.graphs.transformations.Transformation
import dahu.model.ir.{ExprF, Total}

package object transformations {

  val totalPasses: Seq[Pass[Total]] = Pass.allTotalPasses

  val optimizer: Transformation[Total, Total] = makeOptimizer[Total](totalPasses)

  def makeOptimizer[F[X] >: Total[X] <: ExprF[X]](passes: Seq[Pass[F]]): Transformation[F, F] =
    new Transformation[F, F] {
      override def transformation[I <: Int](_retrieve: I => F[I],
                                            _record: F[I] => I): F[I] => F[I] = {
        val ctx = new OptimizationContext[I, F] {
          override def retrieve(i: I): F[I] = _retrieve(i)
          override def record(fi: F[I]): I = _record(fi)
        }
        val pass: Pass[F] = new ComposedPass[F](passes)
        new ASTOptimizer[I, F](ctx, pass)
      }
    }
}
