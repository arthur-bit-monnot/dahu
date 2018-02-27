package dahu.constraints

import dahu.constraints.Constraint.Updater
import dahu.constraints.domains._
import dahu.model.types._
import dahu.utils.errors.unexpected
import spire.math.Trilean
import dahu.utils.structures._
import dahu.constraints.interval._
import dahu.maps.mutable.MArrayMap
import dahu.maps.{ArrayMap, SubInt}
import dahu.model.ir.AST
import dahu.model.problem.SatisfactionProblem
import dahu.solvers.problem.{IntCSP, IntProblem}
import dahu.solvers.problem.IntProblem.{Comp, Func}
import dahu.solvers.{DomainIso, Solver}
import spire.implicits._
import dahu.utils.debug._

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class Event[T <: SubInt]
final case class SolutionFound[T <: SubInt](sol: ArrayMap.Aux[T, Int]) extends Event[T]
class DomainUpdate[T <: SubInt](val id: T, val domain: Interval, val previous: Interval)
    extends Event[T]
case class Inference[T <: SubInt](override val id: T,
                                  override val domain: Interval,
                                  override val previous: Interval)
    extends DomainUpdate[T](id, domain, previous)
case class LocalDecision[T <: SubInt](override val id: T,
                                      override val domain: Interval,
                                      override val previous: Interval)
    extends DomainUpdate[T](id, domain, previous)
case class ExternalDecision[T <: SubInt](override val id: T,
                                         override val domain: Interval,
                                         override val previous: Interval)
    extends DomainUpdate(id, domain, previous)

object Constraint {

  type Updater[T <: SubInt] = CSP[T] => Seq[Inference[T]]

  private val emptyUpdateSingleton = Array[Inference[Nothing]]()
  def emptyUpdate[T <: SubInt] = emptyUpdateSingleton.asInstanceOf[Array[Inference[T]]]

  def fromForward[T <: SubInt](id: T, args: Array[T], prop: ForwardPropagator): Updater[T] = {
    (csp: CSP[T]) =>
      {
        val d = prop.propagate(args, csp.dom)
        val d2 = d inter csp.dom(id)
        if(csp.dom(id) != d2) {
          Array(Inference(id, d2, csp.dom(id)))
        } else {
          emptyUpdate[T]
        }
      }
  }
  def fromBackward[T <: SubInt](id: T, args: Array[T], prop: BackwardPropagator): Updater[T] = {
    (csp: CSP[T]) =>
      {
        val doms = prop.propagate(args, id, csp.dom)
        assert(doms.size == args.size)
        args
          .zip(doms)
          .map { case (i, d) => (i, d inter csp.dom(i)) }
          .filter { case (i, d) => csp.dom(i) != d }
          .map { case (i, d) => Inference(i, d, csp.dom(i)) }
          .toArray[Inference[T]]
      }
  }
}

class CSP[K <: SubInt](params: ArrayMap.Aux[K, IntProblem.Expr], conjunct: Set[K])
    extends Solver[K, Int, Interval] {
  type Var = K
  type Vars = Array[Var]

  val ids: debox.Buffer[Var] = params.domain.toSortedBuffer
  def initialDomains(v: Var): IntDomain = params(v).domain
  def dag(v: Var): Option[Comp] = params(v).comp

  override def domainIso: DomainIso[Interval, Int] = DomainIso.intervalIso

  val propagators: Array[Array[Updater[K]]] = {
    val propagatorsBuilder = Array.fill(ids.max + 1)(mutable.ArrayBuffer[Updater[K]]())
    for(i <- ids) {
      dag(i) match {
        case Some(Comp(Func(_, fw, bw), untaggedArgs)) =>
          val args = untaggedArgs.asInstanceOf[Vars]
          val forwardUpdater = Constraint.fromForward(i, args, fw)
          val backwardUpdater = Constraint.fromBackward(i, args, bw)
          for(a <- args) {
            propagatorsBuilder(a) += forwardUpdater
            propagatorsBuilder(a) += backwardUpdater
          }
          propagatorsBuilder(i) += backwardUpdater
        case _ =>
        // not a computation, do nothing
      }
    }
    propagatorsBuilder.map(buff => if(buff.isEmpty) Array.empty[Updater[K]] else buff.toArray)
  }

  //TODO: DO INITIAL PROPAGATION, OTHERWISE WE MIGHT MISS SOME CONSTRAINTS !

  require(ids.forall(initialDomains(_) != null))
//  require(ids.forall(i => propagators(i) != null && propagators(i).nonEmpty))

  // current domains
  private val domains: MArrayMap.Aux[K, Interval] =
    params.map {
      case IntProblem.Expr(d: IntervalDomain, _, _) => Interval(d.lb, d.ub)
      case _ =>
        unexpected(
          "There might be holes in this domain, for which implementation is not complete currently.")
    }.toMutable

  def dom(v: K): Interval = domains(v)

  override def consistent: Trilean =
    if(solution) Trilean.True
    else if(arcConsistent) Trilean.Unknown
    else Trilean.False

  private def arcConsistent: Boolean = ids.forall(i => !dom(i).isEmpty)
  private def solution: Boolean = ids.forall(i => dom(i).isSingleton)

  private val history = mutable.ArrayBuffer[Event[K]]()
  private def pop(): Option[Event[K]] = {
    if(history.nonEmpty)
      Some(history.remove(history.size - 1))
    else
      None
  }
  private def push(e: Event[K]) = {
    history += e
  }

  for(c <- conjunct) {
    assert(domains.hasKey(c))
    //domains.update(c, BooleanDomain.True)
    enforce(ExternalDecision(c, BooleanDomain.True, dom(c)))
  }

  /** Returns true if the search space was exhausted */
  @tailrec final private def backtrack(): Option[LocalDecision[K]] = {
    pop() match {
      case Some(Inference(i, _, previous)) =>
        domains(i) = previous
        backtrack()
      case Some(backtrackPoint: LocalDecision[K]) =>
        Some(backtrackPoint)
      case Some(SolutionFound(_)) =>
        backtrack()
      case _ =>
        None
    }
  }

  override def enforce(variable: K, domain: Interval): Unit = {
    require(dom(variable).contains(domain))
    val up = ExternalDecision(variable, domain, dom(variable))
    enforce(up)
  }

  /** Enforces an updates and run inference.
    * Returns false if a variable was given the empty domain. */
  final private def enforce(update: DomainUpdate[K]): Boolean = {
    assert(update.domain != update.previous)
    domains(update.id) = update.domain
    push(update)
    if(update.domain.isEmpty)
      return false

    val head = update match {
      case x: LocalDecision[K]    => "decision: "
      case x: Inference[K]        => "   infer: "
      case x: ExternalDecision[K] => "     ext: "
    }
    info(s"$head ${update.id} <- ${update.domain.show}       (prev: ${update.previous.show})")

    val props = propagators(update.id)
    for(p <- props) {
      val ups = p(this)
      for(u <- ups) {
        if(!enforce(u))
          return false
      }
    }
    true
  }

  override def nextSolution(): Option[Assignment] = {
    var exhausted = false
    while(!exhausted) {
      info(ids.map(i => s"$i: ${dom(i).show}").toIterable().mkString("domains: [", ",   ", "]"))
      if(solution && !history.lastOption.forall(_.isInstanceOf[SolutionFound[K]])) {
        // just reached new solution
        info("solution")
        assert(arcConsistent)
        val sol = domains.map(_.lb).toImmutable
        push(SolutionFound[K](sol))
        return Some(sol)
      } else if(!arcConsistent || solution) {
        // inconsistent or already explored solution
        info("backtrack")
        backtrack() match {
          case Some(LocalDecision(v, dec, previous)) =>
            assert(!previous.strictlyContains(dec), "The without approximation is identity.")
            val restricted = previous withoutApproximation dec
            val e = Inference(v, restricted, previous)
            enforce(e)

          case None =>
            exhausted = true
        }
      } else {
        // consistent but not a solution
        // sort variable by domain size and lower bound of the domain
        val vars: debox.Buffer[Var] = ids.sortedBy(v => (math.min(dom(v).size, 100), dom(v).lb)) // to array is necessary because sortBy returns an array of objects
        val variable: Option[Var] = vars.toIterable().collectFirst {
          case i if dom(i).isEmpty      => unexpected("Empty domain in consistent CSP")
          case i if !dom(i).isSingleton => i
        }
        val v = variable.getOrElse(unexpected("CSP is not a solution but all variables are set."))
        val decision = LocalDecision(v, Interval(dom(v).lb), dom(v))
        info(s"Decision: $v <- ${decision.domain.show}")
        enforce(decision)
      }
    }

    None
  }

  override def extractSolution(assignment: Assignment): Solution = {
    assignment.mapFromKey { case (k, v) => params(k).typ.toValue(v) }
  }

  def enumerateSolutions(maxSolutions: Option[Int] = None,
                         onSolutionFound: Assignment => Unit = _ => ()): Int = {
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

object CSP {

  def from(ast: AST[_]) = {
    val sat = SatisfactionProblem.satisfactionSubAST(ast)
    val x = IntCSP.intProblem(sat)
    x.getSolver
  }
}
