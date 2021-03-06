package dahu.planning.planner.encoding
import dahu.model.input.Expr
import dahu.planning.model.common.{Arg, LocalVar}

trait VariableResolver {
  def getArg(a: Arg): Expr[Literal]
  def getLocalVar(v: LocalVar): Expr[Literal]
}
