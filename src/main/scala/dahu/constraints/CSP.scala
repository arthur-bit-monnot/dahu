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
import dahu.solver.Solver
import dahu.utils.Errors.unexpected
import spire.math.Trilean
import dahu.utils.structures._
import spire.implicits._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

sealed abstract class Event[T]
class DomainUpdate[T](val id: KI[T], val domain: IntDomain, val previous: IntDomain) extends Event[T]
case class Inference[T](override val id: KI[T],
                     override val domain: IntDomain,
                     override val previous: IntDomain)
    extends DomainUpdate[T](id, domain, previous)
case class LocalDecision[T](override val id: KI[T],
                         override val domain: IntDomain,
                         override val previous: IntDomain)
    extends DomainUpdate[T](id, domain, previous)
case class ExternalDecision[T](override val id: KI[T],
                    override val domain: IntDomain,
                    override val previous: IntDomain)
    extends DomainUpdate(id, domain, previous)

object Constraint {

  type Updater[T] = CSP[T] => Seq[Inference[T]]

  private val emptyUpdateSingleton = Array[Inference[Nothing]]()
  def emptyUpdate[T] = emptyUpdateSingleton.asInstanceOf[Array[Inference[T]]]

  def fromForward[T](id: KI[T], args: Array[KI[T]], prop: ForwardPropagator): Updater[T] = { (csp: CSP[T]) =>
    {
      val d  = prop.propagate(args, csp.dom)
      val d2 = d & csp.dom(id)
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
          .map { case (i, d) => (i, d & csp.dom(i)) }
          .filter { case (i, d) => csp.dom(i) != d }
          .map { case (i, d) => Inference(i, d, csp.dom(i)) }
          .toArray[Inference[T]]
      }
  }
}

trait IntFunc[V] {
  type T
  type Key = KI[T]
  protected def wrap(i: Int): Key = i.asInstanceOf[Key]
  protected def wrapF[F[_]](f: F[Int]): F[Key] = f.asInstanceOf[F[Key]]

  def domain: debox.Set[Key]
  def apply(key: Key): V
  def isInDomain(i: Int): Boolean = domain(wrap(i))

  def get(key: Int): Option[V] = if(domain(wrap(key))) Some(apply(wrap(key))) else None
}
object IntFunc {
  type Aux[T0, V] = IntFunc[V] { type T = T0 }
}
trait MutableIntFunc[@specialized V] extends IntFunc[V] {
  // todo: remove and provide builder instances instead.
  // extending allows to by pass type safety, as there is no guarantee that two IntFunc with the same Key type share the same set of keys
  def extend(key: Int, value: V)
  def update(key: Key, value: V)
}
class MutableMapIntFunc[@specialized V: ClassTag] extends MutableIntFunc[V] {
  private val map = debox.Map[Key, V]()
  override def domain = map.keysSet
  override def apply(value: Key): V = map(value)
  override def extend(key: Int, value: V): Unit = {
    assert(!map.contains(wrap(key)))
    map.update(wrap(key), value)
  }
   override def update(key: Key, value: V): Unit = {
    assert(map.contains(key))
    map.update(key, value)
  }
}
class MutableArrayIntFunc[@specialized V: ClassTag: Default] extends MutableIntFunc[V] {
  private val keys: debox.Set[Key] = debox.Set[Key]()
  private val buff = debox.Buffer[V]()

  override def domain: debox.Set[Key] = keys.copy()
  override def apply(value: Key): V = buff(value)

  override def extend(key: Int, value: V): Unit = {
    if(!keys.add(wrap(key)))
      throw new IllegalArgumentException(s"Key $key is already recorded.")

    val defaultValue = Default.of[V]
    while(buff.length <= key)
      buff.append(defaultValue)
    buff(key) = value
  }

  override def update(key: Key, value: V): Unit = buff(key) = value
}
object MutableArrayIntFunc {
  type Aux[T0,V] = MutableIntFunc[V] { type T = T0 }
  def map[T0,A,B: ClassTag: Default](intFunc: IntFunc.Aux[T0, A])(f: A => B): Aux[T0,B] = {
    val tmp = new MutableArrayIntFunc[B] {
      type T = T0
    }
    for(x <- intFunc.domain)
      tmp.extend(x, f(intFunc(x)))
    tmp
  }
}

class CSP[T](params: IntFunc.Aux[T, (IntDomain, Option[Comp])]) extends Solver[T, Int, IntDomain] {
  type Var = KI[T]
  type Vars = Array[Var]
  val ids: debox.Buffer[Var] = debox.Buffer.fromIterable(params.domain.toIterable())
  def initialDomains(v: Var): IntDomain = params(v)._1
  def dag(v: Var): Option[Comp] = params(v)._2

  val propagators: Array[Array[Updater[T]]] = {
    val propagatorsBuilder = Array.fill(ids.max +1)(mutable.ArrayBuffer[Updater[T]]())
    for (i <- ids) {
      dag(i) match {
        case Some(Comp(Func(_, fw, bw), untaggedArgs)) =>
          val args =  untaggedArgs.asInstanceOf[Vars]
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
    propagatorsBuilder.map(buff => if(buff.isEmpty) Array.empty[Updater[T]] else buff.toArray)
  }

  require(ids.forall(initialDomains(_) != null))
  require(ids.forall(i => propagators(i) != null && propagators(i).nonEmpty))

  // current domains
  private val domains: MutableArrayIntFunc.Aux[T, IntDomain] =
    MutableArrayIntFunc.map(params)((x: (IntDomain, Option[Comp])) => x._1)

  def dom(v: Var): IntDomain = domains(v)

  override def consistent: Trilean =
    if(solution) Trilean.True
    else if(arcConsistent) Trilean.Unknown
    else Trilean.False

  private def arcConsistent: Boolean = ids.forall(i => !dom(i).isEmpty)
  private def solution: Boolean   = ids.forall(i => dom(i).isSingleton)

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
      case _ =>
        None
    }
  }

  override def enforce(variable: Var, domain: IntDomain): Unit = {
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
      case x: LocalDecision[T]  => "decision: "
      case x: Inference[T] => "   infer: "
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

  def solve: Option[IntFunc.Aux[T, Int]] = {
    if(!arcConsistent)
      return None

    var exhausted = false
    while(!exhausted) {
      println(ids.map(i => s"$i: ${dom(i)}").toIterable().mkString("domains: [", ",   ", "]"))
      if(solution) {
        println("solution")
        assert(arcConsistent)
        val domainsBackup = MutableArrayIntFunc.map(domains)(_.head)
        return Some(domainsBackup)
      } else if(!arcConsistent) {
        println("backtrack")
        backtrack() match {
          case Some(LocalDecision(v, dec, previous)) =>
            val restricted = previous \ dec
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
        val decision = LocalDecision(v, SingletonDomain(dom(v).lb), dom(v))
        println(s"Decision: $v <- ${decision.domain}")
        enforce(decision)
      }
    }

    None
  }

}







object CSP {


  def from(asg: ASDAG[_]): CSP[_] = {
    val x = IntCSP.intSubProblem(asg)(_ => true)
    x.getSolver
  }
}