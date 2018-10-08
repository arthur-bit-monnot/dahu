package dahu.solvers.problem
import dahu.model.input.Expr

trait DynamicCSP[Result] {

  def constraints: Expr[Boolean]
  def exports: Seq[(Expr[Any], Expr[Boolean])]
  def result: Seq[Expr[Result]]

}

trait StaticCSP[Result] {
  def constraints: Expr[Boolean]
  def result: Seq[Expr[Result]]
}
