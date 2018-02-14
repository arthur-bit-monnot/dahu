package dahu.constraints

import dahu.arrows.recursion.Coalgebra
import dahu.constraints.Constraint.Updater
import dahu.constraints.IntProblem.{Comp, Func, Var}
import dahu.constraints.domains._
import dahu.recursion._
import dahu.expr._
import dahu.expr.types.TagIsoInt
import dahu.interpreter.domains.Domain
import dahu.recursion.Types._
import dahu.solver.Solver
import dahu.utils.Errors.unexpected

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class Event
class DomainUpdate(val id: ExprId, val domain: IntDomain, val previous: IntDomain) extends Event
case class Inference(override val id: ExprId,
                     override val domain: IntDomain,
                     override val previous: IntDomain)
    extends DomainUpdate(id, domain, previous)
case class LocalDecision(override val id: ExprId,
                         override val domain: IntDomain,
                         override val previous: IntDomain)
    extends DomainUpdate(id, domain, previous)
case class ExternalDecision(override val id: ExprId,
                    override val domain: IntDomain,
                    override val previous: IntDomain)
    extends DomainUpdate(id, domain, previous)

object Constraint {

  type Updater = CSP => Seq[Inference]

  private val emptyUpdate = Array[Inference]()

  def fromForward(id: Var, args: Array[Var], prop: ForwardPropagator): Updater = { (csp: CSP) =>
    {
      val d  = prop.propagate(args, csp.dom)
      val d2 = d & csp.dom(id)
      if(csp.dom(id) != d2) {
        Array(Inference(id, d2, csp.dom(id)))
      } else {
        emptyUpdate
      }
    }
  }
  def fromBackward(id: Var, args: Array[Var], prop: BackwardPropagator): Updater = {
    (csp: CSP) =>
      {
        val doms = prop.propagate(args, id, csp.dom)
        assert(doms.size == args.size)
        args
          .zip(doms)
          .map { case (i, d) => (i, d & csp.dom(i)) }
          .filter { case (i, d) => csp.dom(i) != d }
          .map { case (i, d) => Inference(i, d, csp.dom(i)) }
          .toArray[Inference]
      }
  }
}

class CSP(val initialDomains: Var => IntDomain, val dag: Var => Option[Comp], val ids: Array[ExprId]) extends Solver[Int, IntDomain] {

  val propagators: Array[Array[Updater]] = {
    val propagatorsBuilder = Array.fill(ids.max +1)(mutable.ArrayBuffer[Updater]())
    for (i <- ids) {
      dag(i) match {
        case Some(Comp(Func(_, fw, bw), args)) =>
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
    propagatorsBuilder.map(buff => if(buff.isEmpty) Array.empty[Updater] else buff.toArray)
  }

  require(ids.forall(initialDomains(_) != null))
  require(ids.forall(i => propagators(i) != null && propagators(i).nonEmpty))

  // current domains
  private val domains: Array[IntDomain] = {
    val domainsBuilder = Array.fill[IntDomain](ids.max +1)(null)
    for(i <- ids)
      domainsBuilder(i) = initialDomains(i)
    domainsBuilder
  }
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
  @tailrec final private def backtrack(): Option[LocalDecision] = {
    pop() match {
      case Some(Inference(i, _, previous)) =>
        domains(i) = previous
        backtrack()
      case Some(backtrackPoint: LocalDecision) =>
        Some(backtrackPoint)
      case _ =>
        None
    }
  }

  override def enforce(variable: ExprId, domain: IntDomain): Unit = {
    require(dom(variable).contains(domain))
    val up = ExternalDecision(variable, domain, dom(variable))
    enforce(up)
  }

  /** Enforces an updates and run inference.
    * Returns false if a variable was given the empty domain. */
  final private def enforce(update: DomainUpdate): Boolean = {
    assert(update.domain != update.previous)
    domains(update.id) = update.domain
    push(update)
    if(update.domain.isEmpty)
      return false

    val head = update match {
      case x: LocalDecision  => "decision: "
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
          case Some(LocalDecision(v, dec, previous)) =>
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
        val decision = LocalDecision(v, SingletonDomain(dom(v).lb), dom(v))
        println(s"Decision: $v <- ${decision.domain}")
        enforce(decision)
      }
    }

    None
  }

}







object IntProblem {
  import Types._
  type Var = ExprId
  type Val = Int

  type Vars = Array[Var]
  type Vals = Array[Val]

//  type Func = Vals => Val
  case class Func(eval: Vals => Val, fw: ForwardPropagator, bw: BackwardPropagator)

  /** Computation is a function applied to some variables. */
  case class Comp(f: Func, params: Vars)

  case class Expr(domain: IntDomain, comp: Option[Comp])
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

    def getSolver: CSP
  }
  object IntCSP {
    def domainOfType(typ: TagIsoInt[_]): IntervalDomain = IntervalDomain(typ.min, typ.max)

    def translate(id: Var, coalgebra: Var => ExprF[Var]): Option[IntProblem.Expr] = {
      import dahu.expr.labels.Labels.Value

      def asIntFunction(f: Fun[_]): Option[Func] = {
        // todo: cache generated function
        IntCompatibleFunc.compat(f)
          .map(icl => (args: Vals) => {
            icl.outToInt(Value(f.compute(icl.argsFromInt(args))))
          })
          .map(i2iFunc => Func(i2iFunc, Propagator.forward(f), Propagator.backward(f)))
      }
      coalgebra(id) match {
        case InputF(_, typ: TagIsoInt[_]) =>
          Some(Expr(domainOfType(typ), None))
        case CstF(value, typ: TagIsoInt[_]) =>
          Some(Expr(SingletonDomain(typ.toIntUnsafe(value)), None))
        case ComputationF(f, args, typ: TagIsoInt[_]) =>
          val expr: Option[Comp] = asIntFunction(f).map(fi => Comp(fi, args.toArray))
          Some(Expr(domainOfType(typ), expr))
        case _ =>
          None

      }
    }
  }

  def intSubProblem(asg: ASDAG[_])(candidates: ExprId => Boolean): IntCSP = {
    val variables = mutable.ArrayBuffer[Var]()
    val domains = new Array[IntDomain](asg.ids.last.value+1)
    val expressions = new Array[Comp](asg.ids.last.value+1)
    asg.ids.enumerate
      .map(i => IntCSP.translate(i, asg.coalgebra.asScalaFunction).map((i, _)))
      .foreach {
        case Some((i, expr)) if candidates(i) =>
          variables += i
          if(i == asg.root)
            domains(i) = True // todo: should be set externally
          else
            domains(i) = expr.domain

          expressions(i) = // only treat as computation if it is representable and is in the candidate set
            expr.comp
              .flatMap(c => if(candidates(i)) Some(c) else None)
              .orNull
        case _ => // not representable or not in candidate set, ignore
      }
    val externalInputs: Set[Var] = {
      variables.toSet
        .flatMap((v: Var) => asg.coalgebra(v) match {
          case ComputationF(_, args, _) => args.toSet
          case _ => Set[Var]()
        })
        .filterNot(candidates)
    }
    for(v <- externalInputs) {
      assert(!variables.contains(v))
      assert(domains(v) == null)
      variables += v
      domains(v) = asg.coalgebra(v).typ match {
        case t: TagIsoInt[_] => IntCSP.domainOfType(t)
        case _ => unexpected("Some computation in IntCSP depends on an expression whose type is not isomorphic to Int.")
      }
    }
    new IntCSP {
      override def dom: Var => IntDomain = v => domains(v)
      override def exprs: Var => Option[Comp] = v => Option(expressions(v))
      override def vars: Array[Var] = variables.toArray

      override def getSolver: CSP =
        new CSP(dom, exprs, vars)
    }
  }

  def from(asg: ASDAG[_]): CSP = {
    val x = intSubProblem(asg)(_ => true)
    x.getSolver
  }
}