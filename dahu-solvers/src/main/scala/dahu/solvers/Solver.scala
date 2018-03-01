package dahu.solvers

import dahu.constraints.domains.IntervalDomain
import dahu.constraints.interval.Interval
import dahu.maps.{ArrayMap, SubInt}
import dahu.model.ir.{AST, InputF, TotalSubAST}
import dahu.model.problem.SatisfactionProblem
import dahu.model.types._
import dahu.solvers.problem.IntCSP
import dahu.utils.errors._
import spire.math.Trilean

trait Domain[V] {
  def iterable: Iterable[V] = ???
}

trait DomainIso[D, V] {

  def to(d: Domain[V]): D
  def from(d: D): Domain[V]
}
object DomainIso {
  private val identityIso = new DomainIso[Domain[Any], Any] {
    override def to(d: Domain[Any]): Domain[Any] = d
    override def from(d: Domain[Any]): Domain[Any] = d
  }
  def identity[V]: DomainIso[Domain[V], V] = identityIso.asInstanceOf[DomainIso[Domain[V], V]]

  val intervalIso: DomainIso[Interval, Int] = new DomainIso[Interval, Int] {
    override def to(d: Domain[Int]): Interval = d match {
      case x: IntervalDomain => Interval(x.lb, x.ub)
      case _                 => unexpected("No completly faithfull translation of non interval domains")
    }
    override def from(d: Interval): IntervalDomain = IntervalDomain(d.lb, d.ub)
  }
}

trait Solver[K <: SubInt, V, D] {
  type Assignment = ArrayMap.Aux[K, V]
  type Solution = ArrayMap.Aux[K, Value]

  def domainIso: DomainIso[D, V]

  def enforce(variable: K, domain: D)
  def nextSolution(): Option[Assignment]

  def extractSolution(assignment: Assignment): Solution

  def consistent: Trilean
}

abstract class PartialSolver[AST <: TotalSubAST[_]](final val ast: AST) {

  type K <: ast.ID

  def nextSatisfyingAssignment(): Option[ast.PartialAssignment]
}

class CSPPartialSolver[AST <: TotalSubAST[_]](_ast: AST) extends PartialSolver[AST](_ast) {
  override type K = IntCSP.Key[ast.ID]

  private val intPB = IntCSP.intProblem(ast)
  private val csp: Solver[K, Int, Interval] = intPB.getSolver

  override def nextSatisfyingAssignment(): Option[ast.PartialAssignment] = {
    csp.nextSolution() match {
      case Some(ass) =>
        val sol = csp.extractSolution(ass)
        val partial: ast.PartialAssignment = (k: ast.VID) => {
          sol.get(k)
        }
        Some(partial)
      case None => None
    }
  }
}
object PartialSolver {

  trait Builder {
    def apply[AST <: TotalSubAST[_]](ast: AST): PartialSolver[AST]
  }

  object CSPBuilder extends Builder {
    override def apply[AST <: TotalSubAST[_]](ast: AST): PartialSolver[AST] =
      new CSPPartialSolver[AST](ast)
  }
}

class Meta[K <: SubInt](val ast: AST.Aux[_, K], val builder: PartialSolver.Builder) {
  val sat = SatisfactionProblem.satisfactionSubAST(ast)
  val solver: PartialSolver[sat.type] = builder(sat)
  val x: sat.type = solver.ast
  implicitly[sat.type =:= solver.ast.type]

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
  def of[X](ast: AST[X]): Meta[ast.ID] = new Meta(ast, PartialSolver.CSPBuilder)
}
