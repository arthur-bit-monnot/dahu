package dahu.constraints

import dahu.arrows.recursion.Coalgebra
import dahu.constraints.Constraint.Updater
import dahu.constraints.domains._
import dahu.recursion._
import dahu.expr._
import dahu.expr.types.TagIsoInt
import dahu.interpreter.domains.Domain
import dahu.utils.Errors.unexpected
import io.estatico.newtype.NewType

import scala.annotation.tailrec
import scala.collection.mutable

abstract class Constraint {
  def propagate(csp: CSP): Unit
}

sealed abstract class Event
class DomainUpdate(val id: Int, val domain: IntDomain, val previous: IntDomain) extends Event
case class Inference(override val id: Int,
                     override val domain: IntDomain,
                     override val previous: IntDomain)
    extends DomainUpdate(id, domain, previous)
case class Decision(override val id: Int,
                    override val domain: IntDomain,
                    override val previous: IntDomain)
    extends DomainUpdate(id, domain, previous)

object Constraint {

  type Updater = CSP => Seq[Inference]

  private val emptyUpdate = Array[Inference]()

  def fromForward(id: Int, c: ComputationF[Int], prop: ForwardPropagator): Updater = { (csp: CSP) =>
    {
      val d  = prop.propagate(c.args, csp.dom)
      val d2 = d & csp.dom(id)
      if(csp.dom(id) != d2) {
        Array(Inference(id, d2, csp.dom(id)))
      } else {
        emptyUpdate
      }
    }
  }
  def fromBackward(id: Int, c: ComputationF[Int], prop: BackwardPropagator): Updater = {
    (csp: CSP) =>
      {
        val doms = prop.propagate(c.args, id, csp.dom)
        assert(doms.size == c.args.size)
        c.args
          .zip(doms)
          .map { case (i, d) => (i, d & csp.dom(i)) }
          .filter { case (i, d) => csp.dom(i) != d }
          .map { case (i, d) => Inference(i, d, csp.dom(i)) }
          .toArray[Inference]
      }
  }
}

trait Solver[EId] {
  def solve: Option[EId => Int]
}


class CSP(val initialDomains: Array[IntDomain], val propagators: Array[Array[Updater]], val root: Int, val ids: Array[Int]) {

  require(ids.contains(root))
  require(ids.forall(initialDomains(_) != null))
  require(ids.forall(i => propagators(i) != null && propagators(i).nonEmpty))

  type Var = Int
  // current domains
  private val domains: Array[IntDomain] = initialDomains.clone()
  def dom(v: Var): IntDomain = domains(v)




  private def consistent: Boolean = ids.forall(i => !dom(i).isEmpty)
  private def solution: Boolean   = ids.forall(i => dom(i).isSingleton)

  private val history = mutable.ArrayBuffer[Event]()
  private def pop(): Option[Event] = {
    if(history.nonEmpty)
      Some(history.remove(history.size - 1))
    else
      None
  }
  private def push(e: Event) = {
    history += e
  }

  /** Returns true if the search space was exhausted */
  @tailrec final def backtrack(): Option[Decision] = {
    pop() match {
      case Some(Inference(i, _, previous)) =>
        domains(i) = previous
        backtrack()
      case Some(backtrackPoint: Decision) =>
        Some(backtrackPoint)
      case None =>
        None
    }
  }

  /** Enforces an updates and run inference.
    * Returns false if a variable was given the empty domain. */
  final def enforce(update: DomainUpdate): Boolean = {
    assert(update.domain != update.previous)
    domains(update.id) = update.domain
    push(update)
    if(update.domain.isEmpty)
      return false

    val head = update match {
      case x: Decision  => "decision: "
      case x: Inference => "   infer: "
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

  def solve: Option[Var => Int] = {
    if(!consistent)
      return None

    var exhausted = false
    while(!exhausted) {
      println(ids.map(i => s"$i: ${dom(i)}").mkString("domains: [", ",   ", "]"))
      if(solution) {
        println("solution")
        assert(consistent)
        val domainsBackup = domains.clone() // save solution
        return Some(x => domainsBackup(x).head)
      } else if(!consistent) {
        println("backtrack")
        backtrack() match {
          case Some(Decision(v, dec, previous)) =>
            val restricted = previous \ dec
            val e          = Inference(v, restricted, previous)
            enforce(e)

          case None =>
            exhausted = true
        }
      } else {
        // consistent but not a solution

        // a sort variable by domain size and lower bound of the domain
        val vars = ids.sortBy(v => (math.min(dom(v).size, 100), dom(v).lb))
        val variable: Option[Var] = vars.collectFirst {
          case i if dom(i).isEmpty      => unexpected("Empty domain in consistent CSP")
          case i if !dom(i).isSingleton => i
        }
        val v        = variable.getOrElse(unexpected("CSP is not a solution but all variables are set."))
        val decision = Decision(v, SingletonDomain(dom(v).lb), dom(v))
        //        println(s"Decision: ${asg.coalgebra(v)} <- ${decision.domain}")
        enforce(decision)
      }
    }

    None
  }

}

object IntProblem {

  type Var = Var.Type
  object Var extends NewType.Of[Int]

//  type Val = Val.Type
//  object Val extends NewType.Of[Int]
  type Val = Int

  type Vars = Vars.Type
  object Vars extends NewType.Of[Array[Var]] {
    def apply(vars: Array[Var]) = vars.clone().asInstanceOf[Vars]
  }

  type Vals = Vals.Type
  object Vals extends NewType.Default[Array[Val]] {
    implicit class Ops(val v: Vals) extends AnyVal {
      def unwrap: Array[Val] = v.asInstanceOf[Array[Val]]
    }
  }

  type Func = Vals => Val

  /** Computation is a function applied to some variables. */
  type Comp = (Func, Vars)

  type Expr = (IntDomain, Option[Comp])
}
import IntProblem._
object CSP {


  trait Problem[V] {
    def vars: Array[Var]
    def hasVar(v: Var): Boolean
    def dom: Var => Option[Domain[V]]

  }

  abstract class IntCSP {
    def vars: Array[Var]
    def dom: Var => IntDomain
    def exprs: Var => Option[Comp]
  }
  object IntCSP {
    def translate(id: Var, coalgebra: Var => ExprF[Var]): Option[IntProblem.Expr] = {
      import dahu.expr.labels.Labels.Value
      def domainOfType(typ: TagIsoInt[_]): IntervalDomain = IntervalDomain(typ.min, typ.max)
      def asIntFunction(f: Fun[_]): Option[Vals => Val] = {
        IntCompatibleFunc.compat(f).map(icl => (args: Vals) => {
          icl.outToInt(Value(f.compute(icl.argsFromInt(args.unwrap))))
        })
      }
      coalgebra(id) match {
        case InputF(_, typ: TagIsoInt[_]) =>
          Some((domainOfType(typ), None))
        case CstF(value, typ: TagIsoInt[_]) =>
          Some((SingletonDomain(typ.toIntUnsafe(value)), None))
        case ComputationF(f, args, typ: TagIsoInt[_]) =>
          val expr: Option[Comp] = asIntFunction(f).map(fi => (fi, Vars(args.toArray)))
          Some((domainOfType(typ), expr))
//          asIntFunction(f)
//            .map(fi => (domainOfType(typ), Some((fi, args.toArray))))
        case _ =>
          None

      }
    }
  }

  def intSubProblem(asg: ASDAG[_])(candidates: asg.EId => Boolean): IntCSP = {
    implicit def eid2int(id: asg.EId): Int              = asg.EId.toInt(id)
    implicit def eid2intF[F[_]](id: F[asg.EId]): F[Int] = asg.EId.toIntF(id)
    val variables = mutable.ArrayBuffer[Var]()
    val domains = new Array[IntDomain](asg.ids.last+1)
    val exprs = new Array[Option[Comp]](asg.ids.last+1)
//    for(i <- asg.ids.enumerate) {
//      IntCSP.translate(i, )
//    }

    new IntCSP {

      override def dom: Var => IntDomain = ???
      override def exprs: Var => Option[(Func, Vars)] = ???
      override def vars: Array[Var] = ???
    }
  }

  def from(asg: ASDAG[_]): Solver[asg.EId] = {
    import scala.language.implicitConversions
    implicit def eid2int(id: asg.EId): Int              = asg.EId.toInt(id)
    implicit def eid2intF[F[_]](id: F[asg.EId]): F[Int] = asg.EId.toIntF(id)

    val dom: Array[IntDomain] = new Array(asg.ids.last + 1)
    for(i <- asg.ids.enumerate) {
      if(i == asg.root) {
        dom(i) = True // root is always true
      } else {
        val d = asg.coalgebra(i) match {
          case CstF(value, t: TagIsoInt[_]) => SingletonDomain(t.toIntUnsafe(value))
          case x if x.typ.isInstanceOf[TagIsoInt[_]] =>
            IntervalDomain(
              x.typ.asInstanceOf[TagIsoInt[_]].min,
              x.typ.asInstanceOf[TagIsoInt[_]].max
            )
        }
        dom(i) = d
      }
    }

    // populate propagators
    val propagators: Array[mutable.ArrayBuffer[Updater]] =
      Array.fill(asg.ids.last + 1)(mutable.ArrayBuffer())
    for(i <- asg.ids.enumerate) {
      asg.coalgebra(i) match {
        case c @ ComputationF(f, args, _) =>
          val fw            = Propagator.forward(f)
          val fwUp: Updater = Constraint.fromForward(i, c, fw)
          val bw            = Propagator.backward(f)
          val bwUp: Updater = Constraint.fromBackward(i, c, bw)
          for(a <- c.args) {
            propagators(a) += fwUp
            propagators(a) += bwUp
          }
          propagators(i) += bwUp
        case _ =>
      }
    }

    val csp = new CSP(dom, propagators.map(_.toArray), asg.root, asg.ids.enumerate)

    new Solver[asg.EId] {
      override def solve: Option[asg.EId => Int] =
        csp.solve.map(f => (x => f(x)))
    }
  }
}