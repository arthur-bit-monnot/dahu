package dahu.model.problem

import java.util

import algebra.Order
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.model.compiler.Algebras
import dahu.model.ir.{ComputationF, CstF, Total}
import dahu.model.math.bool
import dahu.model.types.Tag
import dahu.utils._

import scala.annotation.switch

object Types {
  trait BoolDomMarker
  trait VarMarker
  type Var = SInt[VarMarker]

  type Dom = SInt[BoolDomMarker]
  final val NOT_BOOL: Dom = (-3).asInstanceOf[Dom]
  final val UNKNOWN: Dom = (-2).asInstanceOf[Dom]
  final val EMPTY: Dom = (-1).asInstanceOf[Dom]
  final val FALSE: Dom = 0.asInstanceOf[Dom]
  final val TRUE: Dom = 1.asInstanceOf[Dom]

  trait VarPairMarker { self: Long =>
  }
  type VarPair = Long with VarPairMarker
  import spire._
  import spire.implicits._
  implicit val classTag: ClassTag[VarPair] =
    implicitly[ClassTag[Long]].asInstanceOf[ClassTag[VarPair]]
  implicit val order: Order[VarPair] = Order[Long].asInstanceOf[Order[VarPair]]

  object VarPair {
    def apply(a: Int, b: Int): VarPair = {
      if(a > b) {
        apply(b, a)
      } else {
        val l = a.toLong << 32 | b.toLong & 0xFFFFFFFFL
        l.asInstanceOf[VarPair]
      }
    }
  }

  implicit final class IntervalOps(val lhs: VarPair) extends AnyVal {
    def left: Int = (lhs >> 32).toInt
    def right: Int = lhs.toInt
    def show: String = s"($left, $right)"
  }
}

import Types._

class Processor[X, ID <: IDTop] {

  def inter(d1: Dom, d2: Dom): Dom = {
    if(d1 == d2)
      d1
    else if(d1 > d2)
      inter(d2, d1)
    else
      d1 match {
        case NOT_BOOL => errors.unexpected
        case UNKNOWN  => d2
        case EMPTY    => EMPTY
        case FALSE    => EMPTY // d2 is necessarily true
        case TRUE     => errors.unexpected
      }
  }
  def format(dom: Dom): String =
    dom match {
      case NOT_BOOL => "NOT_BOOL"
      case UNKNOWN  => "ANY"
      case EMPTY    => "EMPTY"
      case FALSE    => "FALSE"
      case TRUE     => "TRUE"
    }
  def not(dom: Dom): Dom = {
    dom match { // in theory, we should be able to translate that to a switch, but it does not work in practice
      case FALSE => TRUE
      case TRUE  => FALSE
      case x     => x
    }
  }
  def inverse(v: Var): Var = {
    // note: this is terrible allocation-wise
    coalg(v) match {
      case ComputationF(bool.Not, vs, _) => vs(0)
      case _                             => record(ComputationF(bool.Not, Vec(v), Tag.ofBoolean))
    }
  }

  private val bimap = BiMap[ID, Var]()
  private val coalg = debox.Buffer[Total[Var]]() // Var => Total[Var]
  private val domains = debox.Buffer[Dom]() // Var => Dom
  private val varId = debox.Map[Total[Var], Var]()

  private val inferenceQueue = debox.Buffer[Var]()
  private val inferences = debox.Buffer[Var]()

  val implication = new ImplicationGraph

  def learnFrom(v: Var): Unit = {
    coalg(v) match {
      case ComputationF(f, args, _) =>
        f match {
          case bool.Or if domains(v) == TRUE && args.length == 2 =>
            val a = args(0)
            val b = args(1)
            val notA = inverse(a)
            val notB = inverse(b)
            implication.add(notA, b)
            implication.add(notB, a)
          case _ =>
        }
      case _ =>
    }
  }

  def computeDependencies(v: Var): Unit = {
    coalg(v) match {
      case ga @ ComputationF(f, args, _) =>
        f match {
          case bool.And =>
            args.foreach { arg =>
              implication.add(v, arg)
              implication.add(inverse(arg), inverse(v))
            }
            implication.addConjunct(args, v)
          case bool.Or =>
            args.foreach { arg =>
              implication.add(arg, v)
              implication.add(inverse(v), inverse(arg))
            }
            implication.addConjunct(args.map(inverse), inverse(v))

          case _ =>
        }
      case _ =>
    }
  }

  private def record(ga: Total[Var]): Var = {
    assert(domains.length == coalg.length)
    if(!varId.contains(ga)) {
      coalg.append(ga)
      val id = (coalg.length - 1).asInstanceOf[Var]
      varId.update(ga, id)
      if(ga.typ == Tag.ofBoolean)
        domains.append(UNKNOWN)
      else
        domains.append(NOT_BOOL)
      computeDependencies(id)
      id
    } else {
      varId(ga)
    }
  }

  def load(tree: LazyTree[X, Total, cats.Id, ID]): Unit = {
    val root = tree.tree.getTreeRoot(tree.root)
    val processingOrder = Graph.topologicalOrderLeavesToRoot[ID, Total](
      root,
      id => tree.tree.internalCoalgebra(id),
      fa => TreeNode[Total].children(fa))
    for(id <- processingOrder) {
      val fa = tree.tree.internalCoalgebra(id)
      val ga = fa.smap(i => bimap.get(i))
      val varID = record(ga)
      bimap.add(id, varID)
    }

    val count = debox.Map[ID, Int]()
    for(id <- processingOrder) {
      val fa = tree.tree.internalCoalgebra(id)
      for(x <- fa.children) {
        count(x) = count.getOrElse(x, 0) + 1
      }
    }
    val printTree = tree.tree.cata(Algebras.printAlgebraTree)
    count
      .iterator()
      .filter(_._2 > 1)
      .toSeq
      .sortBy(_._2)
      .reverse
      .foreach {
        case (n, c) =>
          println(s"\n$c $n")
          println(printTree.getInternal(n).mkString(30))
      }
    updateDomain(bimap.get(root), TRUE)
  }

  private def updateDomain(v: Var, newDom: Dom): Unit = {
    val oldD = domains(v)
    val res = inter(oldD, newDom)
    if(res == EMPTY)
      ???
    if(res != oldD) {
      domains(v) = res
      inferenceQueue.append(v)
      learnFrom(v)
      if(res == TRUE) {
        inferences.append(v)
      }
      println(s"inferred: ${format(res)} <- $v: ${coalg(v)}")
    }
  }

  def process(): Unit = {
    def pop(): Var = inferenceQueue.remove(inferenceQueue.length - 1)

    while(inferenceQueue.nonEmpty) {
      val v = pop()
      val d = domains(v)
      assert(d == TRUE || d == FALSE)
      updateDomain(inverse(v), not(d))
      coalg(v) match {
        case ComputationF(f, vars, _) =>
          f match {
            case bool.And if d == TRUE =>
              vars.foreach(updateDomain(_, TRUE))
            case bool.Or if d == FALSE =>
              vars.foreach(updateDomain(_, FALSE))
            case _ =>
          }
        case CstF(true, _)  => updateDomain(v, TRUE)
        case CstF(false, _) => updateDomain(v, FALSE)
        case _              =>
      }
    }
    for(k <- implication.graph.keys.take(1)) {
      println(coalg(k))
      implication.graph(k).foreach(x => println("  " + coalg(x)))
      implication.descendants(k).foreach(x => println("    " + coalg(x)))
    }
    println("a")
  }
}

object Inference2Sat {

  def processTargettingTrue[X](
      tree: LazyTree[X, Total, cats.Id, _]): LazyTree[X, Total, cats.Id, _] = {
    val t = tree.fixID

    val proc = new Processor[X, t.ID]
    proc.load(t)
    proc.process()

    t
  }
}

class ArrayMapWithDefault[K <: SubInt, V: ClassTag](default: () => V) {
  var buff = new Array[V](10)
  var present = new Array[Boolean](10)
  assert(!present(0)) // check that array initializes to false
  def contains(k: K): Boolean = k < present.length && present(k)

  def keys: Iterator[K] = new Iterator[K] {
    var cur = -1
    goToNext()
    def goToNext(): Unit = {
      cur += 1
      while(cur < present.length && !present(cur)) cur += 1
    }
    override def hasNext: Boolean = {
      cur < present.length && present(cur)
    }

    override def next(): K = {
      val k = cur.asInstanceOf[K]
      goToNext()
      k
    }
  }

  private def growTo(k: K): Unit = {
    if(k >= present.length) {
      val newSize = math.max(buff.length, k) * 2
      val newBuff = new Array[V](newSize)
      System.arraycopy(buff, 0, newBuff, 0, buff.length)
      buff = newBuff
      val newPresent = new Array[Boolean](newSize)
      System.arraycopy(present, 0, newPresent, 0, present.length)
      present = newPresent
    }
  }
  def apply(k: K): V = get(k)
  def get(k: K): V = {
    growTo(k)
    if(!present(k)) {
      buff(k) = default()
      present(k) = true
    }
    buff(k)

  }
}

class ImplicationGraph {

  val graph = new ArrayMapWithDefault[Var, debox.Set[Var]](() => debox.Set())
  val reverseGraph = new ArrayMapWithDefault[Var, debox.Set[Var]](() => debox.Set())
  val descendants = new ArrayMapWithDefault[Var, debox.Set[Var]](() => debox.Set())
  // watch => watched: Var => Watcher: Var
  // watcher => numRequired
  // watcher => candidate => pending
  val watchersOf = new ArrayMapWithDefault[Var, debox.Set[Var]](() => debox.Set())
  val requirements = debox.Map[Var, Vec[Var]]()
  val pendings = new ArrayMapWithDefault[Var, debox.Map[Var, Int]](() => debox.Map())
  private def getPendings(watcher: Var, provider: Var): Int = {
    val numReq = requirements(watcher).size
    pendings.get(watcher).getOrElseUpdate(provider, numReq)
  }
  private def decreasePending(watcher: Var, provider: Var): Int = {
    pendings(watcher).update(provider, getPendings(watcher, provider) - 1)
    getPendings(watcher, provider)
  }

  def has(from: Var, to: Var): Boolean =
    graph.contains(from) && graph(from)(to)

  def addAll(from: Var, iterator: Iterable[Var]): Unit = {
    graph(from).addAll(iterator)
  }

  def addConjunct(from: Vec[Var], to: Var): Unit = {
    from.foreach(watchersOf(_) += to)
    assert(!requirements.contains(to))
    requirements(to) = from
  }

  def add(from: Var, to: Var): Unit = {
    if(has(from, to))
      return

    graph(from) += to
    reverseGraph(to) += from
    updateDescendants(from, to)
  }
  def updateDescendants(of: Var, withChild: Var): Unit = {
    val prevSize = descendants(of).size
    addDescendant(of, withChild)
    if(descendants.contains(withChild))
      descendants(withChild).foreach(d => addDescendant(of, d))
    //TODO: use a queue
    if(descendants(of).size > prevSize)
      for(parent <- reverseGraph(of))
        updateDescendants(parent, of)
  }
  private def addDescendant(from: Var, to: Var): Unit = {
    if(!descendants(from)(to)) {
      descendants(from) += to
      watchersOf(to).foreach { w =>
        val pending = decreasePending(w, from)
        if(pending == 0)
          add(from, w)
      }
    }
  }

}
