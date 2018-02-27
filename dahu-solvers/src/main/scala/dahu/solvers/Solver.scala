package dahu.solvers

import dahu.constraints.domains.IntervalDomain
import dahu.constraints.interval.Interval
import dahu.maps.{ArrayMap, SubInt}
import dahu.model.ir.{AST, InputF}
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

class MetaSolver[K <: SubInt](val ast: AST.Aux[_, K]) {

  val sat = SatisfactionProblem.satisfactionSubAST(ast)
  val csp = IntCSP.intProblem(sat).getSolver

  def typeOf(id: K): dahu.model.types.Tag[_] = ast.tree(id).typ

  def defaultDomain(k: ast.VID): Stream[Value] = ast.variables(k) match {
    case InputF(_, t: TagIsoInt[_]) =>
      assert(t.min <= t.max, "empty default domain")
      (t.min to t.max).toStream.map(i => t.toValue(i))
    case _ => ???
  }

  def nextSolution(): Option[ast.Assignment] = csp.nextSolution() match {
    case Some(assignment) =>
      val f = csp.extractSolution(assignment)
      val partial: ast.PartialAssignment = (k: ast.VID) => {
        val k2: Option[sat.ID] = sat.subset.to(k)
        k2.flatMap(i => f.get(i))
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
  def of[X](ast: AST[X]): MetaSolver[ast.ID] = new MetaSolver(ast)
}
