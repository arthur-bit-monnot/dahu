package dahu.constraints

import dahu.constraints.Constraint.Updater
import dahu.constraints.domains._
import dahu.dataframe.metadata.Value
import dahu.recursion.{ASDAG, ComputationF, CstF, InputF}
import dahu.expr._
import dahu.expr.types.TagIsoInt
import dahu.utils.Errors.unexpected

import scala.annotation.tailrec
import scala.collection.mutable

abstract class Constraint {
  def propagate(csp: CSP): Unit
}

sealed abstract class Event
class DomainUpdate(val id: Int, val domain: IntDomain, val previous: IntDomain) extends Event
case class Inference(override val id: Int, override val domain: IntDomain, override val previous: IntDomain) extends DomainUpdate(id, domain, previous)
case class Decision(override val id: Int, override val domain: IntDomain, override val previous: IntDomain) extends DomainUpdate(id, domain, previous)

object Constraint {

  type Updater = CSP => Seq[Inference]

  private val emptyUpdate = Array[Inference]()

  def fromForward(id: Int, c: ComputationF[Int], prop: ForwardPropagator): Updater = {
    (csp: CSP) => {
      val d = prop.propagate(c.args, csp.dom)
      val d2 = d & csp.dom(id)
      if(csp.dom(id) != d2) {
        Array(Inference(id, d2, csp.dom(id)))
      } else {
        emptyUpdate
      }
    }
  }
  def fromBackward(id: Int, c: ComputationF[Int], prop: BackwardPropagator): Updater = {
    (csp: CSP) => {
      val doms = prop.propagate(c.args, id, csp.dom)
      assert(doms.size == c.args.size)
      c.args
        .zip(doms)
        .map { case (i, d) => (i, d & csp.dom(i)) }
        .filter{ case (i, d) => csp.dom(i) != d }
        .map{ case (i, d) => Inference(i, d, csp.dom(i)) }
        .toArray[Inference]
    }
  }
}


class CSP(val asg: ASDAG[_]) {
  import scala.language.implicitConversions
  implicit def eid2int(id: asg.EId): Int = asg.EId.toInt(id)
  implicit def eid2intF[F[_]](id: F[asg.EId]): F[Int] = asg.EId.toIntF(id)

  val dom: Array[IntDomain] = new Array(asg.ids.last+1)
  for(i <- asg.ids.enumerate) {
    if(i == asg.root) {
      dom(i) = True // root is always true
    } else {
      val d = asg.coalgebra(i) match {
        case CstF(value, t: TagIsoInt[_]) => SingletonDomain(t.toIntUnsafe(value))
        case x if x.typ.isInstanceOf[TagIsoInt[_]] => IntervalDomain(
          x.typ.asInstanceOf[TagIsoInt[_]].min,
          x.typ.asInstanceOf[TagIsoInt[_]].max
        )
      }
      dom(i) = d
    }
  }

  // populate propagators
  val propagators: Array[mutable.ArrayBuffer[Updater]] = Array.fill(asg.ids.last+1)(mutable.ArrayBuffer())
  for(i <- asg.ids.enumerate) {
    asg.coalgebra(i) match {
      case c @ ComputationF(f, args, _) =>
        val fw = Propagator.forward(f)
        val fwUp: Updater = Constraint.fromForward(i, c, fw)
        val bw = Propagator.backward(f)
        val bwUp: Updater = Constraint.fromBackward(i, c, bw)
        for(a <- c.args) {
          propagators(a) += fwUp
          propagators(a) += bwUp
        }
        propagators(i) += bwUp
      case _ =>
    }
  }
  private def consistent: Boolean = asg.ids.enumerate.forall(i => !dom(i).isEmpty)
  private def solution: Boolean = asg.ids.enumerate.forall(i => dom(i).isSingleton)

  def solve: Option[asg.EId => Int] = {
    if(!consistent)
      return None


    val history = mutable.ArrayBuffer[Event]()
    def pop(): Option[Event] = {
      if(history.nonEmpty)
        Some(history.remove(history.size-1))
      else
        None
    }
    def push(e: Event) = {
      history += e
    }
    /** Returns true if the search space was exhausted */
    @tailrec def revertLastDecision(): Boolean = {
      pop() match {
        case Some(Inference(i, _, previous)) =>
          dom(i) = previous
          revertLastDecision()
        case Some(Decision(i, dec, previous)) =>
          val restricted = previous \ dec
          val e = Inference(i, restricted, previous)
          dom(i) = restricted
          enforce(e)
          false
        case None =>
          true
      }
    }

    /** Enforces an updates and run inference.
      * Returns false if a variable was given the empty domain. */
    def enforce(update: DomainUpdate): Boolean = {
      assert(update.domain != update.previous)
      dom(update.id) = update.domain
      push(update)
      if(update.domain.isEmpty)
        return false

      val head = update match {
        case x: Decision => "decision: "
        case x: Inference => "  infer: "
      }
      println(s"$head ${asg.coalgebra(update.id.asInstanceOf[asg.EId])} <- ${update.domain}       (prev: ${update.previous})")

      val props = propagators(update.id)
      for(p <- props) {
        val ups = p(this)
        for(u <- ups) {
          val consistent = enforce(u)
          if(!consistent)
            return false
        }
      }
      return true
    }

    var exhausted = false
    while(!exhausted) {
      println(asg.ids.enumerate.map(i => s"$i: ${dom(i)}").mkString("domains: [", ",   ", "]"))
      if(solution) {
        println("solution")
        assert(consistent)
        val domains = dom.clone() // save solution
        return Some(x => domains(x).head)
      } else if(!consistent) {
        println("backtrack")
        exhausted = revertLastDecision()
      } else {
        // consistent but not a solution

        // a sort variable by domain size and lower bound of the domain
        val vars = asg.ids.enumerate.sortBy(v => (math.min(dom(v).size, 100), dom(v).lb))
        val variable: Option[asg.EId] = vars.collectFirst {
          case i if dom(i).isEmpty => unexpected("Empty domain in consistent CSP")
          case i if !dom(i).isSingleton => i
        }
        val v = variable.getOrElse(unexpected("CSP is not a solution but all variables are set."))
        val decision = Decision(v, SingletonDomain(dom(v).lb), dom(v))
//        println(s"Decision: ${asg.coalgebra(v)} <- ${decision.domain}")
        enforce(decision)
      }
    }

    None
  }

}
