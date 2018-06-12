package dahu.model.problem

import algebra.Monoid
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.utils._
import dahu.model.input.{Expr, SubjectTo}
import dahu.model.ir._
import dahu.graphs.TreeNode._
import dahu.model.compiler.Algebras
import dahu.model.input.Lambda.LambdaIdent
import dahu.model.math.bool
import dahu.model.problem.SatisfactionProblem.Utils
import dahu.model.types.Tag
import dahu.recursion.{EnvT, FCoalgebra, Recursion}

import scala.collection.{immutable, mutable}

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

  implicit def treeNode[A, F[_]: TreeNode]: TreeNode[EnvT[A, F, ?]] = new TreeNode[EnvT[A, F, ?]] {
    override def children[K](n: EnvT[A, F, K]): immutable.Iterable[K] = n.lower.children
  }

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

    val lambdaDeps: I => LambdaDeps[I] = forest
      .cataLow[LambdaDeps[I]] {
        case (i, LambdaParamF(id, _)) => LambdaDeps(Map(id -> i), Nil, pure = true)
        case (_, LambdaF(in, ast, id, _)) =>
          assert(ast.params.contains(id))
          ast.copy(applicationStack = id :: ast.applicationStack)
        case (_, ApplyF(lbd, param, _)) =>
          assert(lbd.applicationStack.nonEmpty)
          assert(param.applicationStack.isEmpty)
          val app :: remainindApp = lbd.applicationStack
          val params = (lbd.params ++ param.params) - app
          LambdaDeps(params, remainindApp, lbd.pure && param.pure)
        case (_, x) if x.children.isEmpty   => LambdaDeps.empty
        case (_, x) if x.children.size == 1 => x.children.head
        case (_, Partial(value, cond, _)) =>
          LambdaDeps(value.params ++ cond.params, value.applicationStack, false)
        case (_, OptionalF(value, cond, _)) =>
          LambdaDeps(value.params ++ cond.params, value.applicationStack, false)
        case (_, x) =>
          if(x.children.exists(c => c.applicationStack.nonEmpty))
            println("break")
          val bindings = x.children.map(_.params).fold(Map())(_ ++ _)
          val pure = x.children.map(_.pure).fold(true)(_ && _)
          LambdaDeps(bindings, List(), pure)
      }
      .asInternalFunction

    // I => G[I]
    // J => F[J] -- with J = (I, Set[i])
    val cache = mutable.Map[CI, ExprF[I]]()
    def record(e: ExprF[I], ctx: CtxDef[I]): CI = {
      val ci = (-(cache.size + 1), ctx).asInstanceOf[CI] // AAAARh
      cache.update(ci, e)
      ci
    }
    def genKey(i: I, c: CtxDef[I]): (I, CtxDef[I]) = {
      (i, lambdaDeps(i).filter(c))
    }
//    sealed trait Node[F]
    case class Orig[F](v: NoApplyF[F], cond: Set[F])
    def trans(dyns: Vec[CI]): CI => NoApplyF[CI] = {
      case (i, si) => {
        val fi =
          if(cache.contains((i, si)))
            cache((i, si))
          else
            forest.internalCoalgebra(i)
        fi match {
          case OptionalF(value, present, t) =>
            OptionalF(genKey(value, si + present), genKey(present, si), t) //, (present, si), t)
          case ApplyF(lbd, param, t) =>
            NoopF(genKey(lbd, si.copy(applyStak = param :: si.applyStak)), t)
          case LambdaF(in, ast, _, t) if si.applyStak.nonEmpty =>
            val CtxDef(presence, binds, nextApply :: followingApply) = si
            NoopF(genKey(ast, CtxDef(presence, binds + ((in, nextApply)), followingApply)), t) // note type might not line up, should be tree.type
          case DynamicF(f, monoid, _, t) =>
            val fed = dyns.map(d => record(ApplyF(f, d._1, Tag.default[Any]), d._2))
            ComputationF(monoid, fed, t)
          case DynamicProviderF(e, p, t) =>
//            assert(dyns.contains((p, si)))
            NoopF(genKey(e, si), t)
          case _ if si.binds.contains(i) =>
            NoopF(genKey(si.binds(i), si), Tag.default[Any])
//          case LambdaF(in, ast, _, t) =>
//            ???
          case fi: NoApplyF[I] => fi.smap(genKey(_, si))
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
    val AST = IlazyForest.ana(trans(dynamics.toVec))
    val back = AST.idsMap.toSeq.groupBy(_._2).mapValues(_.map(_._1._2.conditions))

    println("===========================================================")
    xx.sorted.foreach(println)
    println("STOP")
    val contextualizedRoot: CI = (root, CtxDef())
    val order = Graph.topologicalOrderTopDownPrioritized[CI, ExprF](
      contextualizedRoot,
      i => coalgFinal(i),
      fa => fa.children.iterator,
      k => Int.MaxValue - k._2.conditions.size)

    order.foreach { ci =>
      println(s"$ci    ------     ${coalgFinal(ci)}")
    }
//    type OptExprF[I] = (ExprF[I], ExprF[I])
//    IlazyForest.build[CI, ExprF, OptExprF, cats.Id](k => coalgFinal(k))(ctx => {
//      case x @ InputF(_, _) => ctx.record((x, bool.TrueF))
//
//    })
//    println(
//      Algebras.pprint[CI](i => coalgFinal(i), (root, CtxDef())).mkString(90)
//    )
  }

  case class LambdaDeps[@sp(Int) I](params: Map[LambdaIdent, I],
                                    applicationStack: List[LambdaIdent],
                                    pure: Boolean) {

    lazy val values = params.values.toSet

    def map[@sp(Int) B](f: I => B): LambdaDeps[B] =
      LambdaDeps(params.mapValues(f), applicationStack, pure)

    private def filterParams(ctx: CtxDef[I]): CtxDef[I] = ctx match {
      case _ if applicationStack.nonEmpty =>
        ctx
      case CtxDef(conds, binds, applyStak) if !binds.keys.forall(k => values.contains(k)) =>
        CtxDef(conds, binds.filter { case (_, v) => values.contains(v) })
      case _ =>
        ctx
    }
    private def filterContext(ctx: CtxDef[I]): CtxDef[I] =
      if(pure) ctx.copy(conditions = Set[I]())
      else ctx

    def filter(ctx: CtxDef[I]): CtxDef[I] = filterContext(filterParams(ctx))
  }

  object LambdaDeps {
    private val singleton = LambdaDeps[Int](Map(), List(), pure = true)
    def empty[I <: Int]: LambdaDeps[I] = singleton.asInstanceOf[LambdaDeps[I]]
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
//    memo.map { case (k, v) => s"$k   --    $v" }.toSeq.sorted.foreach(println)
    curAcc
  }

  def process3[K](_tree: LazyTree[K, ExprF, cats.Id, _]): Unit = {
//    import dahu.recursion._
//    val tree = API.expandLambdas(API.eliminitateDynamics(_tree)).fixID
//    val forest = tree.tree
//    val treeRoot = tree.tree.getTreeRoot(tree.root)
//    type ID = tree.tree.ID
//    type Opt[I] = (I, Set[I])
//
//    type Node[I] = (NoApplyF[I], I)
//    val coalg: FCoalgebra[NoApplyF, Opt[ID]] = {
//      case (i, si) => {
//        val fi = tree.tree.internalCoalgebra(i)
//        fi match {
//          case x: OptionalF[_] => x.smap((_, si + i))
//          case x               => x.smap((_, si))
//        }
//      }
//    }
//    val contexts = traverseAcc(coalg)((treeRoot, Set()), List[(Any, Set[ID])]()) {
//      case ((i, x: InputF[_]), l) =>
//        (x, i._2) :: l
//      case ((i, x: CstF[_]), l) =>
//        (x, i._2) :: l
//      case (_, l) => l
//    }
//    val contextMap = contexts.groupBy(_._1).mapValues(_.map(_._2).toSet).toMap //.foreach(println)
//    case class IR[@sp(Int) K](value: K, presence: K)

//    forest.mapInternalGen[IR](ctx => {
//      def formatCond(ssi: Set[Set[ID]]): Total[IDTop] = {
//        val disjuncts =
//          ssi.map(si =>
//            ctx.record(ComputationF(bool.And, si.map(x => ctx.toNewId).toVec, Tag.ofBoolean)))
//        ComputationF(bool.Or, disjuncts.toVec, Tag.ofBoolean)
//      }
//      _ match {
//        case x @ InputF(v, t) => IR(ctx.record(x), formatCond(contextMap(x)))
//      }
//    })
//    type OptExpr[K] = (NoApplyF[K], Option[K])
//    val coalg2: FCoalgebra[OptExpr, ID] = {
//      case (i, si) => {
//        val fi = tree.tree.internalCoalgebra(i)
//        fi match {
//          case OptionalF(v, p, t) => (NoopF(v, t), Some(p))
//          case x                  => x.smap((_, si))
//        }
//      }
//    }
//
//    case class Group()
//    val attCoalg = coalg.toAttributeCoalgebra
//    val attAlg: AttributeAlgebra[Opt[ID], NoApplyF, Set[ID]] = {
//      case EnvT(ctx,  default) => default.children.fold(ctx._2)(_ ++ _)
//      case EnvT(ctx, PresentF(_)) => Set()
//      case EnvT(ctx, ITEF())
//    }
//    val ana = IlazyForest.ana(attCoalg).fixID
//    type ANAEnv[X] = EnvT[Opt[ID], NoApplyF, X]
//    type X[I] = (Total[I], Total[I])
//    ana.mapInternalGen[X](ctx => {
//      def format(c: Set[ana.ID]): Total[IDTop] =
//        ComputationF(bool.And, c.map(ctx.toNewId).toVec, Tag.ofBoolean)
//      val ret: ANAEnv[IDTop] => X[IDTop] = {
//        case EnvT(ctx, default: Total[IDTop]) => (default, format(ctx._2))
//        case EnvT(ctx, PresentF(x))           => (???, ???)
//      }
//      ret
//    })
//      IlazyForest
//        .ana[Opt[ID], NoApplyF] {
//          case (i, si) => {
//            val fi = tree.tree.internalCoalgebra(i)
//            fi match {
//              case x: OptionalF[_] => x.smap((_, si + i))
//              case x               => x.smap((_, si))
//            }
//          }
//        }
//        .fixID
//    val cata = ana.

//    val accumulator = mutable.Map[IDTop, mutable.Set[IDTop]]()
//    val cata = ana.mapInternal[Node]({
//      case Partial(value, condition, _) =>  ana.
//    })
//
//    println(
//      ana.cata(Algebras.printAlgebraTree).get((treeRoot, Set())).mkString(90)
//    )

  }

}
