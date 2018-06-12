package dahu.model.problem

import algebra.Monoid
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.utils._
import dahu.model.input.{Expr, SubjectTo}
import dahu.model.ir._
import dahu.graphs.TreeNode._
import dahu.model.compiler.Algebras
import dahu.model.types.Tag
import dahu.recursion.Recursion

import scala.collection.mutable

case class Context[I <: IDTop](as: Seq[I] = Seq[I]()) {
  def +(i: I): Context[I] = Context(as :+ i)
  def nesting: Int = as.size

  override def toString: String = as.mkString("Ctx(", ", ", ")")
}
case class ContextSet[I <: IDTop](ctxs: Set[Context[I]]) {
  def combine(o: ContextSet[I]): ContextSet[I] = ContextSet(ctxs ++ o.ctxs)
  def map(f: Context[I] => Context[I]): ContextSet[I] = ContextSet(ctxs.map(f))

  override def toString: String = ctxs.mkString("{", ", ", "}")
}
object ContextSet {
  def pure[I <: IDTop]: ContextSet[I] = ContextSet(Set())

  private object MonoidInstance extends Monoid[ContextSet[IDTop]] {
    override def empty: ContextSet[IDTop] = pure
    override def combine(x: ContextSet[IDTop], y: ContextSet[IDTop]): ContextSet[IDTop] =
      x.combine(y)
  }
  implicit def monoidInstance[I <: IDTop]: Monoid[ContextSet[I]] =
    MonoidInstance.asInstanceOf[Monoid[ContextSet[I]]]
}
case class Scope[K, I <: IDTop](ctx: Context[I], parent: Option[Group[K, I]]) {
  def nesting: Int = ctx.nesting
}

class Group[K, I <: IDTop](val scope: Scope[K, I],
                           forest: IlazyForest[K, ExprF, cats.Id, I],
                           roots: Set[I]) {

  private[this] val _members = mutable.Set[I]()
  private[this] val _limits = mutable.Set[(I, Scope[K, I])]()
  private[this] val _validityConditions = mutable.Set[I]()

  roots.foreach(recursivelyAdd)

  lazy val members: Set[I] = _members.toSet
  lazy val limits: Set[(I, Scope[K, I])] = _limits.toSet

  private def parentHas(i: I): Boolean =
    scope.parent.exists(p => p.members.contains(i) || p.parentHas(i))

  private def recursivelyAdd(root: I): Unit = {
    val queue = debox.Buffer[I]()
    var nextQueueIndex = 0
    def pop(): I = {
      nextQueueIndex += 1
      queue(nextQueueIndex - 1)
    }
    def queueEmpty: Boolean = nextQueueIndex >= queue.length
    def push(i: I): Unit = {
      if(!_members(i) && !parentHas(i)) queue.append(i)
    }
    push(root)
    while(!queueEmpty) {
      val i = pop()
      if(!_members(i)) { // if not processed already
        _members.add(i)
        val fi = forest.internalCoalgebra(i)
        fi match {
          case OptionalF(value, present, _) =>
            push(present)
            _limits.add((value, Scope(scope.ctx + present, Some(this))))
          case Partial(value, condition, _) =>
            _validityConditions.add(condition)
            fi.children.foreach(push)
          case _ =>
            fi.children.foreach(push)
        }

      }
    }
  }
}

object Group {
  implicit class SetOps[K, V](private val lhs: mutable.Map[K, V]) extends AnyVal {
    def updateWithDefault(k: K, f: V => V, default: => V): Unit = {
      lhs.update(k, f(lhs.getOrElse(k, default)))
    }
  }

  def process[K](_tree: LazyTree[K, ExprF, cats.Id, _]): Unit = {
    val tree = _tree.fixID
    type I = tree.ID
    val forest = tree.tree
    val root = forest.getTreeRoot(tree.root)

    object Queue extends Iterator[(Scope[K, I], Set[I])] {
      def append(x: (I, Scope[K, I])): Unit = append(x._1, x._2)
      def append(i: I, scope: Scope[K, I]): Unit = pending.append((i, scope))
      private[this] var pending = mutable.ArrayBuffer[(I, Scope[K, I])]()
      override def hasNext: Boolean = pending.nonEmpty
      override def next() = {
        val ctx = pending.iterator.map(_._2).minBy(_.nesting)
        val roots = pending.iterator.collect { case (i, c) if c == ctx => i }.toSet
        pending = pending.filterNot(_._2 == ctx)
        (ctx, roots)
      }
    }
    val rootContext = Scope[K, I](Context[I](), None)
    Queue.append(root, rootContext)

    val groups: Seq[Group[K, I]] = (for((ctx, roots) <- Queue) yield {

      val g = new Group[K, I](ctx, forest, roots)
      g.limits.foreach(Queue.append)

      println(s"${g.scope.ctx} size: ${g.members.size}")
      g
    }).toList
    groups.foreach(_ => ())

    for(g <- groups) {
      println(s"Group: ${g.scope}")
      for(i <- g.members) {
        println(s"   $i:  ${forest.internalCoalgebra(i)}")
      }
    }
//    def nextContextToProcess(): Option[Context[I]] =
//
//
//    def getProc(c: Context[I]): Group[K, I] =
//      contextProcessors.getOrElseUpdate(c, new Group[K, I](c, forest))
//    val rootProc = getProc(rootContext)
//    rootProc.recursivelyAdd(root)
//
//    println("STOP")
//    val contexts = mutable.Map[I, ContextSet[I]]()
//    contexts.update(root, ContextSet(Set(Context[I]())))
//
//    val nodes = forest.internalBottomUpTopologicalOrder(root).toSeq.reverse
//    for(i <- nodes) {
//      val fi = forest.internalCoalgebra(i)
//      assert(contexts.contains(i))
//      println(s"${contexts(i)}  ---------------- $i $fi")
//      val selfCtx = contexts(i)
//      fi match {
//        case OptionalF(value, present, _) =>
//          val sub = selfCtx.map(_ + present)
//          contexts.updateWithDefault(present, _.combine(selfCtx), ContextSet.pure)
//          contexts.updateWithDefault(value, _.combine(sub), ContextSet.pure)
//        case _ =>
//          for(child <- fi.children) {
//            contexts.updateWithDefault(child, _.combine(selfCtx), default = ContextSet.pure)
//          }
//      }
//    }
  }
  case class Expr[I](f: ExprF[I], ctx: CtxDef[I])

  case class CtxDef[I](conditions: Set[I] = Set[I](),
                       binds: Map[I, I] = Map[I, I](),
                       applyStak: List[I] = List[I]()
//                       applyStak: List[(I, CtxDef[I])] = List[(I, CtxDef[I])]()
  ) {
    // points to roots of total contextualized expressions
    def +(i: I): CtxDef[I] = CtxDef(conditions + i, binds)
    //    def \(o: CtxDef[I]): CtxDef[I] = CtxDef(conditions -- o.conditions)

    def show: String = s"(${conditions.mkString("{", ",", "}")} $binds   $applyStak)"
    override def toString: String = s"${conditions.mkString("{", ",", "}")}"
  }
  case class Validity[I](ctx: CtxDef[I], e: Expr[I])
  def process2[K](_tree: LazyTree[K, ExprF, cats.Id, _]): Unit = {
    val tree = _tree.fixID
    type I = tree.ID
    val forest = tree.tree
    val root = forest.getTreeRoot(tree.root)
    type CI = (I, CtxDef[I]) // contextualized index
//    val f: I => ExprF[I] = forest.internalCoalgebra
    // I => G[I]
    // J => F[J] -- with J = (I, Set[i])
    val cache = mutable.Map[CI, ExprF[I]]()
    def record(e: ExprF[I], ctx: CtxDef[I]): CI = {
      val ci = (-(cache.size + 1), ctx).asInstanceOf[CI] // AAAARh
      cache.update(ci, e)
      ci
    }
    def trans(dyns: Vec[CI]): CI => NoApplyF[CI] = {
      case (i, si) => {
        val fi =
          if(cache.contains((i, si)))
            cache((i, si))
          else
            forest.internalCoalgebra(i)
        fi match {
          case OptionalF(value, present, t) =>
            OptionalF((value, si + present), (present, si), t) //, (present, si), t)
          case ApplyF(lbd, param, t) =>
            NoopF((lbd, si.copy(applyStak = param :: si.applyStak)), t)
          case LambdaF(in, ast, _, t) if si.applyStak.nonEmpty =>
            val CtxDef(presence, binds, nextApply :: followingApply) = si
            NoopF((ast, CtxDef(presence, binds + ((in, nextApply)), followingApply)), t) // note type might not line up, should be tree.type
          case DynamicF(f, monoid, _, t) =>
            val fed = dyns.map(d => record(ApplyF(f, d._1, Tag.default[Any]), d._2))
            ComputationF(monoid, fed, t)
          case DynamicProviderF(e, p, t) =>
//            assert(dyns.contains((p, si)))
            NoopF((e, si), t)
          case _ if si.binds.contains(i) =>
            NoopF((si.binds(i), si), Tag.default[Any])
//          case LambdaF(in, ast, _, t) =>
//            ???
          case fi: NoApplyF[I] => fi.smap((_, si))
        }
      }
    }
    // I => Ctx[I]
    // I => Exprf[I]
    def traverse[I, F[_]: TreeNode](coalg: I => F[I])(root: I)(onNew: (I, F[I]) => Unit): Unit = {
      val memo = mutable.Map[I, F[I]]()
      val queue = mutable.Queue[I]()
      queue += root
      while(queue.nonEmpty) {
        val cur = queue.dequeue()
        if(!memo.contains(cur)) {
          val fcur = coalg(cur)
          memo(cur) = fcur
          onNew(cur, fcur)
          for(child <- fcur.children)
            queue += child
        }
      }
//      memo.map { case (k, v) => s"$k   --    $v" }.toSeq.sorted.foreach(println)
    }

    val traverser: CI => ExprF[CI] = {
      case (i, si) => {
        forest.internalCoalgebra(i) match {
          case OptionalF(value, present, t) =>
            OptionalF((value, si + present), (present, si), t) //, (present, si), t)
          case fi => fi.smap((_, si))
        }
      }
    }

    def traverseAcc[Acc, I, F[_]: TreeNode](coalg: I => F[I])(root: I, acc: Acc)(
        fAcc: ((I, F[I]), Acc) => Acc): Acc = {
      val memo = mutable.Map[I, F[I]]()
      val queue = mutable.Queue[I]()
      var curAcc = acc
      queue += root
      while(queue.nonEmpty) {
        val cur = queue.dequeue()
        if(!memo.contains(cur)) {
          val fcur = coalg(cur)
          memo(cur) = fcur
          curAcc = fAcc((cur, fcur), curAcc)
          for(child <- fcur.children)
            queue += child
        }
      }
      memo.map { case (k, v) => s"$k   --    $v" }.toSeq.sorted.foreach(println)
      curAcc
    }
    val dynamics = traverseAcc[List[CI], CI, ExprF](traverser)((root, CtxDef()), Nil) {
      case ((_, DynamicProviderF(_, x, _)), acc) => x :: acc
      case (_, acc)                              => acc
    }
    val xx = mutable.Buffer[String]()
    val coalgFinal = mutable.Map[CI, ExprF[CI]]()
    traverse(trans(dynamics.toVec))((root, CtxDef())) {
      case ((i, ctx), fi) =>
        coalgFinal += (((i, ctx), fi))
        xx += s"$i ${ctx.show} -- $fi --  "
    }
    println("===========================================================")
    xx.sorted.foreach(println)
    println("STOP")
//    println(
//      Algebras.pprint[CI](i => coalgFinal(i), (root, CtxDef())).mkString(90)
//    )
  }

}
