package dahu.solvers

import dahu.maps.SubInt
import dahu.model.ir.{AST, InputF}
import dahu.model.problem.SatisfactionProblem
import dahu.model.types.{TagIsoInt, Value}
import dahu.solvers.constraints.CSPPartialSolver

class MetaSolver[K <: SubInt](val ast: AST.Aux[_, K], val builder: PartialSolver.Builder) {
  val sat = SatisfactionProblem.satisfactionSubAST(ast)
  val solver = builder(sat)

  def typeOf(id: K): dahu.model.types.Tag[_] = ast.tree(id).typ

  def defaultDomain(k: ast.VID): Stream[Value] = ast.variables(k) match {
    case InputF(_, t: TagIsoInt[_]) =>
      assert(t.min <= t.max, "empty default domain")
      (t.min to t.max).toStream.map(i => t.toValue(i))
    case _ => ???
  }

  def nextSolution(): Option[ast.Assignment] = solver.nextSatisfyingAssignment() match {
    case Some(assignment) =>
      val f: sat.PartialAssignment = assignment
      val partial: ast.PartialAssignment = (k: ast.VID) => {
        val k2: Option[sat.ID] = sat.subset.to(k)
        val k3: Option[sat.VID] = k2.flatMap(sat.asVariableID)
        k3.flatMap(i => f(i))
      }
      val total: ast.Assignment = (x: ast.VID) =>
        partial(x) match {
          case Some(v) => v
          case None =>
            defaultDomain(x).head // TODO: use head option or fail early if an input has an empty domain
      }
      Some(total)
    case None => None
  }

  def enumerateSolutions(maxSolutions: Option[Int] = None,
                         onSolutionFound: ast.Assignment => Unit = _ => ()): Int = {
    var count = 0
    while(maxSolutions.forall(count < _)) {
      nextSolution() match {
        case Some(sol) =>
          onSolutionFound(sol)
          count += 1
        case None =>
          return count
      }
    }
    count
  }
}

object MetaSolver {
  def of[X](ast: AST[X], builder: PartialSolver.Builder): MetaSolver[ast.ID] =
    new MetaSolver(ast, builder)
}
