package dahu.constraints

import java.util

import algebra.Order
import dahu.arrows.recursion.Coalgebra
import dahu.constraints.Constraint.Updater
import dahu.constraints.domains._
import dahu.recursion._
import dahu.expr._
import dahu.expr.types.TagIsoInt
import dahu.interpreter.domains.Domain
import dahu.problem.{IntCSP, IntProblem}
import dahu.problem.IntProblem.{Comp, Func, Var}
import dahu.recursion.Types._
import dahu.solver.{DomainIso, Solver}
import dahu.structures.{ArrayIntFunc, MutableArrayIntFunc}
import dahu.utils.Errors.unexpected
import spire.math.Trilean
import dahu.utils.structures._
import dahu.constraints.interval._
import dahu.solver
import spire.implicits._

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class Event[T]
final case class SolutionFound[T](sol: ArrayIntFunc[T, Int])                       extends Event[T]
class DomainUpdate[T](val id: KI[T], val domain: Interval, val previous: Interval) extends Event[T]
case class Inference[T](override val id: KI[T],
                        override val domain: Interval,
                        override val previous: Interval)
    extends DomainUpdate[T](id, domain, previous)
case class LocalDecision[T](override val id: KI[T],
                            override val domain: Interval,
                            override val previous: Interval)
    extends DomainUpdate[T](id, domain, previous)
case class ExternalDecision[T](override val id: KI[T],
                               override val domain: Interval,
                               override val previous: Interval)
    extends DomainUpdate(id, domain, previous)

object Constraint {

  type Updater[T] = CSP[T] => Seq[Inference[T]]

  private val emptyUpdateSingleton = Array[Inference[Nothing]]()
  def emptyUpdate[T]               = emptyUpdateSingleton.asInstanceOf[Array[Inference[T]]]

  def fromForward[T](id: KI[T], args: Array[KI[T]], prop: ForwardPropagator): Updater[T] = {
    (csp: CSP[T]) =>
      {
        val d  = prop.propagate(args, csp.dom)
        val d2 = d inter csp.dom(id)
        if(csp.dom(id) != d2) {
          Array(Inference(id, d2, csp.dom(id)))
        } else {
          emptyUpdate[T]
        }
      }
  }
  def fromBackward[T](id: KI[T], args: Array[KI[T]], prop: BackwardPropagator): Updater[T] = {
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

class CSP[T](params: ArrayIntFunc[T, (IntDomain, Option[Comp])]) extends Solver[T, Int, Interval] {
  type Var        = KI[T]
  type Vars       = Array[Var]
  type Assignment = ArrayIntFunc[T, Int]

  val ids: debox.Buffer[Var]            = params.domain.toSortedBuffer
  def initialDomains(v: Var): IntDomain = params(v)._1
  def dag(v: Var): Option[Comp]         = params(v)._2

  override def domainIso: DomainIso[Interval, Int] = DomainIso.intervalIso

  val propagators: Array[Array[Updater[T]]] = {
    val propagatorsBuilder = Array.fill(ids.max + 1)(mutable.ArrayBuffer[Updater[T]]())
    for(i <- ids) {
      dag(i) match {
        case Some(Comp(Func(_, fw, bw), untaggedArgs)) =>
          val args            = untaggedArgs.asInstanceOf[Vars]
          val forwardUpdater  = Constraint.fromForward(i, args, fw)
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
    propagatorsBuilder.map(buff => if(buff.isEmpty) Array.empty[Updater[T]] else buff.toArray)
  }

  require(ids.forall(initialDomains(_) != null))
  require(ids.forall(i => propagators(i) != null && propagators(i).nonEmpty))

  // current domains
  private val domains: MutableArrayIntFunc[T, Interval] =
    params.map { case (d, _) => Interval(d.lb, d.ub) }.toMutable

  def dom(v: Var): Interval = domains(v)

  override def consistent: Trilean =
    if(solution) Trilean.True
    else if(arcConsistent) Trilean.Unknown
    else Trilean.False

  private def arcConsistent: Boolean = ids.forall(i => !dom(i).isEmpty)
  private def solution: Boolean      = ids.forall(i => dom(i).isSingleton)

  private val history = mutable.ArrayBuffer[Event[T]]()
  private def pop(): Option[Event[T]] = {
    if(history.nonEmpty)
      Some(history.remove(history.size - 1))
    else
      None
  }
  private def push(e: Event[T]) = {
    history += e
  }

  /** Returns true if the search space was exhausted */
  @tailrec final private def backtrack(): Option[LocalDecision[T]] = {
    pop() match {
      case Some(Inference(i, _, previous)) =>
        domains(i) = previous
        backtrack()
      case Some(backtrackPoint: LocalDecision[T]) =>
        Some(backtrackPoint)
      case Some(SolutionFound(_)) =>
        backtrack()
      case _ =>
        None
    }
  }

  override def enforce(variable: Var, domain: Interval): Unit = {
    require(dom(variable).contains(domain))
    val up = ExternalDecision(variable, domain, dom(variable))
    enforce(up)
  }

  /** Enforces an updates and run inference.
    * Returns false if a variable was given the empty domain. */
  final private def enforce(update: DomainUpdate[T]): Boolean = {
    assert(update.domain != update.previous)
    domains(update.id) = update.domain
    push(update)
    if(update.domain.isEmpty)
      return false

    val head = update match {
      case x: LocalDecision[T] => "decision: "
      case x: Inference[T]     => "   infer: "
    }
//    println(
//      s"$head ${asg.coalgebra(update.id.asInstanceOf[asg.EId])} <- ${update.domain}       (prev: ${update.previous})")

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
//      println(ids.map(i => s"$i: ${dom(i).show}").toIterable().mkString("domains: [", ",   ", "]"))
      if(solution && !history.lastOption.forall(_.isInstanceOf[SolutionFound[T]])) {
        // just reached new solution
//        println("solution")
        assert(arcConsistent)
        val sol = domains.map(_.lb).toImmutable
        push(SolutionFound[T](sol))
        return Some(sol)
      } else if(!arcConsistent || solution) {
        // inconsistent or already explored solution
//        println("backtrack")
        backtrack() match {
          case Some(LocalDecision(v, dec, previous)) =>
            assert(!previous.strictlyContains(dec), "The without approximation is identity.")
            val restricted = previous withoutApproximation dec
            val e          = Inference(v, restricted, previous)
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
        val v        = variable.getOrElse(unexpected("CSP is not a solution but all variables are set."))
        val decision = LocalDecision(v, Interval(dom(v).lb), dom(v))
//        println(s"Decision: $v <- ${decision.domain.show}")
        enforce(decision)
      }
    }

    None
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

  def from(asg: ASDAG[_]): CSP[_] = {
    val x = IntCSP.intSubProblem(asg)(_ => true)
    x.getSolver
  }
}
