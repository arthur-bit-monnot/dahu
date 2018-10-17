package dahu.model.transformations
import dahu.graphs.RecursiveTransformation
import dahu.model.ir.{ExprF, Total}

final class ASTOptimizer[I <: Int, F[X] >: Total[X] <: ExprF[X]](ctx: OptimizationContext[I, F],
                                                                 pass: Pass[F])
    extends (F[I] => F[I]) {
  def retrieve(i: I): F[I] = ctx.retrieve(i)
  def record(fi: F[I]): I = {
    if(current != null && current == fi)
      throw RecursiveTransformation
    else {
      ctx.record(fi)
    }
  }

  protected def optimImpl(fi: F[I]): F[I] = pass.optim(ctx).apply(fi)

  private var current: F[I] = null
  def apply(fi: F[I]): F[I] = {
    current = fi
    try {
      optimImpl(fi)
    } catch {
      case RecursiveTransformation => fi
    }
  }
}
