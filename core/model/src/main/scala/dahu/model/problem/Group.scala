package dahu.model.problem

import cats._
import cats.implicits._
import cats.syntax._
import algebra.Monoid
import cats.arrow.FunctionK
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.utils._
import dahu.model.input.Expr
import dahu.model.types._
import dahu.model.ir._
import dahu.graphs.TreeNode._
import dahu.model.compiler.Algebras
import dahu.model.compiler.Algebras.StringTree
import dahu.model.input.Lambda.LambdaIdent
import dahu.model.math.bool
import dahu.model.problem.SatisfactionProblem._
import dahu.model.problem.syntax.{And, Not, Or}
import dahu.model.types.Tag
import dahu.recursion.{EnvT, FCoalgebra, Recursion}

import scala.collection.immutable.Set
import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag

// TODO: (pure) check is still

case class Context[I <: IDTop](as: Seq[I] = Seq[I]()) {
  def +(i: I): Context[I] = Context[I](as :+ i)
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
case class GScope[K, I <: IDTop](ctx: Context[I], parent: Option[Group[K, I]]) {
  def nesting: Int = ctx.nesting
}

class Group[K, I <: IDTop](val scope: GScope[K, I],
                           forest: IlazyForest[K, ExprF, cats.Id, I],
                           roots: Set[I]) {

  private[this] val _members = mutable.Set[I]()
  private[this] val _limits = mutable.Set[(I, GScope[K, I])]()
  private[this] val _validityConditions = mutable.Set[I]()

  roots.foreach(recursivelyAdd)

  lazy val members: Set[I] = _members.toSet
  lazy val limits: Set[(I, GScope[K, I])] = _limits.toSet

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
        fi.foreachChild(push)
      }
    }
  }
}

object Group {

  implicit def treeNode[A, F[_]: TreeNode]: TreeNode[EnvT[A, F, ?]] = new TreeNode[EnvT[A, F, ?]] {
    override def children[K](n: EnvT[A, F, K]): immutable.Iterable[K] = n.lower.children
    override def foreachChild[K](n: EnvT[A, F, K])(f: K => Unit): Unit = n.lower.foreachChild(f)
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

    object Queue extends Iterator[(GScope[K, I], Set[I])] {
      def append(x: (I, GScope[K, I])): Unit = append(x._1, x._2)
      def append(i: I, scope: GScope[K, I]): Unit = pending.append((i, scope))
      private[this] var pending = mutable.ArrayBuffer[(I, GScope[K, I])]()
      override def hasNext: Boolean = pending.nonEmpty
      override def next() = {
        val ctx = pending.iterator.map(_._2).minBy(_.nesting)
        val roots = pending.iterator.collect { case (i, c) if c == ctx => i }.toSet
        pending = pending.filterNot(_._2 == ctx)
        (ctx, roots)
      }
    }
    val rootContext = GScope[K, I](Context[I](), None)
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
//  case class Expr[I](f: ExprF[I], ctx: CtxDef[I])

  case class CtxDef[I](conditions: Set[I] = Set[I](),
                       binds: Map[I, I] = Map[I, I](),
                       applyStak: List[I] = List[I]()
//                       applyStak: List[(I, CtxDef[I])] = List[(I, CtxDef[I])]()
  ) {
    // points to roots of total contextualized expressions
    def +(i: I): CtxDef[I] = CtxDef(conditions + i, binds)
    //    def \(o: CtxDef[I]): CtxDef[I] = CtxDef(conditions -- o.conditions)

    def show: String = s"(${conditions.mkString("{", ",", "}")} $binds   $applyStak)"
    override def toString: String = show //s"${conditions.mkString("{", ",", "}")}"
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
        case (_, x) =>
          val bindings = x.childrenFolLeft(Map[LambdaIdent, I]())(_ ++ _.params) //x.children.map(_.params).fold(Map())(_ ++ _)
          val pure = x.forallChildren(_.pure)
          LambdaDeps(bindings, List(), pure)
      }
      .asInternalFunction

    // I => G[I]
    // J => F[J] -- with J = (I, Set[i])
    val cache = BiMap[CI, ExprF[I]]()
    def record(e: ExprF[I], ctx: CtxDef[I]): CI = {
      if(cache.cocontains(e))
        cache.coget(e)
      else {
        val ci = (-(cache.size + 1), ctx).asInstanceOf[CI] // AAAARh
        cache.add(ci, e)
        ci
      }
    }
    def genKey(i: I, c: CtxDef[I]): (I, CtxDef[I]) = {
      (i, lambdaDeps(i).filter(c))
    }
    //    sealed trait Node[F]
    //    case class Orig[F](v: NoApplyF[F])
    //val memory = mutable.ArrayBuffer[(Set[I], CI)]()
    def trans(dyns: Vec[CI]): CI => Total[CI] = {
      case (i, si) => {
        println((i, si))
        if(i == 123)
          println("BREAK")
        val fi =
          if(cache.contains((i, si)))
            cache.get((i, si))
          else
            forest.internalCoalgebra(i)
        val res = fi match {
          case ApplyF(lbd, param, t) =>
            NoopF(genKey(lbd, si.copy(applyStak = param :: si.applyStak)), t)
          case LambdaF(in, ast, _, t) => //if si.applyStak.nonEmpty =>
            assert(si.applyStak.nonEmpty)
            val CtxDef(presence, binds, nextApply :: followingApply) = si
            NoopF(genKey(ast, CtxDef(presence, binds + ((in, nextApply)), followingApply)), t) // note type might not line up, should be tree.type
          case DynamicF(f, monoid, _, t) =>
            val fed = dyns.map(d => record(ApplyF(f, d._1, monoid.inTypes), d._2))
            ComputationF(monoid, fed, t)
          case _ if si.binds.contains(i) =>
            NoopF(genKey(si.binds(i), si), Tag.default[Any])
//          case LambdaF(in, ast, _, t) =>
//            ???
          case fi: Total[I] =>
            fi.smap(genKey(_, si))
        }
        println(s"$i $si  --  $res")
        assert(res match {
          case x: LambdaParamF[_] => false
          case _                  => true
        })
        res
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
          fcur.foreachChild(queue += _)
        }
      }
//      memo.map { case (k, v) => s"$k   --    $v" }.toSeq.sorted.foreach(println)
    }

    val traverser: CI => ExprF[CI] = {
      case (i, si) => {
        forest.internalCoalgebra(i) match {
          case fi => fi.smap((_, si))
        }
      }
    }

    val dynamics = traverseAcc[List[CI], CI, ExprF](traverser)((root, CtxDef()), Nil) {
      case (_, acc) => acc
    }
    val xx = mutable.Buffer[String]()
    val coalgFinal = mutable.Map[CI, ExprF[CI]]()
//    traverse(trans(dynamics.toVec))((root, CtxDef())) {
//      case ((i, ctx), fi) =>
//        coalgFinal += (((i, ctx), fi))
//        xx += s"$i ${ctx.show} -- $fi --  "
//    }
    val AST = IlazyForest.ana(trans(dynamics.toVec)).fixID
    AST.forceEvaluation((root, CtxDef()))
//    val back = AST.idsMap.toSeq.groupBy(_._2).mapValues(_.map(_._1._2.conditions))

    println("===========================================================")
    xx.sorted.foreach(println)
    println("STOP")
//    val contextualizedRoot: CI = (root, CtxDef())
//    val order = Graph.topologicalOrderTopDownPrioritized[CI, ExprF](
//      contextualizedRoot,
//      i => AST.internalCoalgebra(i),
//      fa => fa.children.iterator,
//      k => Int.MaxValue - k._2.conditions.size)

//    order.foreach { ci =>
//      println(s"$ci    ------     ${coalgFinal(ci)}")
//    }
    println(
      AST.cata(Algebras.printAlgebraTree).get((root, CtxDef())).mkString(90)
    )
    val ASTRoot = AST.getTreeRoot((root, CtxDef()))

    val withPrez = SatisfactionProblem.encodePresence(ASTRoot, AST.internalCoalgebra).fixID
    withPrez.forceEvaluation
    val withPrezForest: IlazyForest[AST.ID, OptConst, cats.Id, withPrez.ID] =
      withPrez.tree.castIDTo[withPrez.ID]

    case class Constraint[A](context: A, condition: A) {
      def map[B](f: A => B): Constraint[B] = Constraint(f(context), f(condition))
      def format: String =
        s"""context:
           [$context
           |condition:
           |$condition
          """.stripMargin
    }

    case class ConstraintSet[A](constraints: Set[Constraint[A]]) {
      def map[B](f: A => B): ConstraintSet[B] = ConstraintSet(constraints.map(_.map(f)))

      def format: String = constraints.map(_.format).mkString("", "\n", "")
    }
    object ConstraintSet {
      private val emptySingleton: ConstraintSet[Any] = new ConstraintSet[Any](Set())
      def empty[F]: ConstraintSet[F] = emptySingleton.asInstanceOf[ConstraintSet[F]]
    }

    val constrained: LazyTree[AST.ID, Total, cats.Id, withPrez.ID] =
      withPrez.map[Total] {
        case OptConst(v, _) => v
      }

    val forgetConstraints: Total ~> Total = lift {
      case x: Total[Something] => x
    }

    val total: LazyTree[AST.ID, Total, cats.Id, withPrez.ID] =
      constrained.mapK[Total](forgetConstraints)
    val totalForest: IlazyForest[AST.ID, Total, cats.Id, withPrez.ID] = total.tree

    val optimized: LazyForestLayer[AST.ID, Total, cats.Id, _, withPrez.ID] =
      totalForest
        .transform(SatisfactionProblem.Optimizations.optimizer)
        .fixID
    val printableOptimized: LazyMap[AST.ID, StringTree, cats.Id, optimized.ID] =
      optimized
        .cata(Algebras.printAlgebraTree)
        .asInstanceOf[LazyMap[AST.ID, StringTree, cats.Id, optimized.ID]]

    type OI = optimized.ID
    implicit val classTag: ClassTag[OI] = ClassTag.Int.asInstanceOf[ClassTag[OI]]
//    implicit def idTrans(i: withPrezForest.ID): optimized.ID = optimized.fromPreviousId(i)
    val constraints = withPrezForest.cataLow2[optimized.ID] {
      case OptConst(expr, context) =>
        val TRUE = optimized.record(bool.TrueF)

        def and(a: OI, b: OI): OI =
          optimized.record(ComputationF(bool.And, Vec(a, b), Tag.ofBoolean))
        def or(a: OI, b: OI): OI =
          optimized.record(ComputationF(bool.Or, Vec(a, b), Tag.ofBoolean))
        def not(a: OI): OI = optimized.record(ComputationF(bool.Not, Vec(a), Tag.ofBoolean))
//        def scopes(a: Constraint[OI]): Vec[OI] = optimized.internalCoalgebra(a.condition) match {
//          case _ if a.condition == true => Vec.empty
//          case ComputationF(bool.And, args, _) => args
//          case _ => Vec(a.condition)
//        }
//        def combine(a: ConstraintSet[optimized.ID],
//                    b: Constraint[optimized.ID]): ConstraintSet[optimized.ID] = {
//          val targetScope = scopes(b)
//          val constraints = a.constraints.map(x => (scopes(x), x)).toSeq.sortBy(_._1.size)
//          def smallerScope(a: Vec[OI], b: Vec[OI]) :Boolean = a.forall(ai => b.contains(ai))
//          val cs = constraints.find(c => smallerScope(scopes(c), targetScope) match {
//            case Some(ca) =>
//              a.constraints.filter(_ != ca) + Constraint(ca.context, and(ca.condition, b.condition))
//            case None => a.constraints + b
//          }
//          ConstraintSet(cs)
//        }
//        def merge(l: List[ConstraintSet[optimized.ID]]): ConstraintSet[optimized.ID] = l match {
//          case Nil            => ConstraintSet.empty
//          case a :: Nil       => a
//          case a :: b :: tail => merge(b.constraints.foldLeft(a)(combine(_, _)) :: tail)
//        }

        val base = expr match {
          case _ => expr.children.map(_._1).fold(TRUE)(and)
        }
        context match {
          case Some((_, i)) => or(not(optimized.fromPreviousId(i)), base)
//            ConstraintSet(base.constraints.map {
//              case Constraint(ctx, cond) => Constraint(and(ctx, optimized.fromPreviousId(i)), cond)
//            })
          case None => base
        }
    }

    val eval = constrained.eval(Algebras.printAlgebraTree)
    println(eval.mkString(90))
    println("================ ")
    println(total.eval(Algebras.printAlgebraTree).mkString(90))
    println("================ ")
    val printableTotal = total.tree.cata(Algebras.printAlgebraTree)

    val c = constraints.get(ASTRoot)
    printableOptimized.getInternal(c)
    println(printableOptimized.getInternal(c).mkString(90))
//    println(eval.present.mkString(90))
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
        CtxDef(conds, binds.filter { case (v, _) => values.contains(v) })
      case _ =>
        ctx
    }
    private def filterContext(ctx: CtxDef[I]): CtxDef[I] =
      if(pure)
        ctx.copy(conditions = Set[I]())
      else
        ctx

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
        fcur.foreachChild(queue += _)
      }
    }
//    memo.map { case (k, v) => s"$k   --    $v" }.toSeq.sorted.foreach(println)
    curAcc
  }

  type Opt[I] = Set[I]
  object Opt {
    def empty[I]: Opt[I] = Set()
  }

  def makeTotal[K](_noLambdas: LazyTree[K, Total, cats.Id, _]): LazyTree[K, Total, IR, _] = {
    val noLambdas = _noLambdas.tree.fixID
    type I = noLambdas.ID

    val presenceAnnotation = noLambdas.cataLow2[Opt[I]] {
      case InputF(_, _) => Opt.empty
      case CstF(_, _)   => Opt.empty
      case ITEF(c, t, f, _) =>
        Opt.empty //TODO: relies on the very questionable assumption that after an ITE, the result is always present.
      case x =>
        // x.children.map(_._1).foldLeft(Opt.empty[I])(_ ++ _)
        x.childrenFolLeft(Opt.empty[I])(_ ++ _._1)
    }

    /** extract constraints applicable to `root`
      * The constraint is of the form `({c1...cn}, v)` which corresponds to a clause
      * `or(not(c1), ..., not(cn), v)`
      * c1...cn correspond to the context (optional objects) in which the constraint `v` appears
      */
    def constraints(root: I): Seq[(Set[I], I)] = {
      val contexts = mutable.Map[I, Set[Set[I]]]()
      val identifiedConstraints = mutable.Map[I, Set[Set[I]]]()
      contexts.update(root, Set(presenceAnnotation.getInternal(root)))

      noLambdas.internalBottomUpTopologicalOrder(root).toSeq.reverse.foreach { i =>
        val fi = noLambdas.internalCoalgebra(i)
        val ctx = contexts(i)

        fi.foreachChild { child =>
          val baseContext = presenceAnnotation.getInternal(child)
          val newContexts = ctx.map(_ ++ baseContext)
          val previousFullContexts = contexts.getOrElse(child, Set())
          contexts.update(child, newContexts ++ previousFullContexts)
        // assert4(contexts(child).map(_.size).min >= baseContext.size)
        }
        // assert(contexts(i).map(_.size).min >= presenceAnnotation.getInternal(i).size)

        fi match {
          case _ =>
        }
      }

      /** minimizes a set of contexts, a context (Set[I]) is only kept if it doesn't contain any other context. */
      def minimize(ssi: Set[Set[I]]): Set[Set[I]] = {
        val x = ssi.toSeq.sortBy(_.size)
        val filtered = mutable.Buffer[Set[I]]()
        for(i <- x.indices) {
          var valid = true

          for(j <- 0 until i) {
            if(x(j).forall(x(i).contains))
              valid = false
          }
          if(valid)
            filtered += x(i)
        }
        filtered.toSet
      }

      val minimized = identifiedConstraints.mapValues(ssi => minimize(ssi))
      minimized.toSeq.flatMap { case (i, ssi) => ssi.toSeq.map((_, i)) }
    }

    val asNodes = noLambdas.mapInternal[Node] {
      case x: Total[I] => Tot(x)
    }
    val finalTree =
      asNodes
        .mapFull[Total, IR](new FullMapGenLazyForest.Generator[Node, Total, cats.Id, IR, I] {
          override def internalMap(ctx: InternalMapGenLazyForest.Context[Node, Total, I, IDTop])(
              fi: Node[IDTop]): Total[IDTop] = fi match {
            case Tot(x)      => x
            case Prezence(z) => And(z)
            case Constraints(cs) =>
              val conjuncts = cs.map {
                case (conds, eff) =>
                  val disjuncts: Set[IDTop] = conds.map(c => ctx.record(Not(c))) + eff
                  ctx.record(Or(disjuncts))
              }
              And(conjuncts)
          }

          override def externalMap(ctx: InternalMapGenLazyForest.Context[Node, Total, I, IDTop])(
              oi: Id[I]): IR[IDTop] = {

            ctx.retrieveOld(oi) match {
              // TODO: we should produce 3 different versions of the lambda and inset a placehold instread of the parameter
              case Tot(p @ LambdaParamF(id, typ))  => ???
              case Tot(LambdaF(in, tree, id, tpe)) => ???
              case _ =>
                val prez = Prezence(presenceAnnotation.getInternal(oi).map(ctx.toNewId))
                val validity = Node.Functor.smap(Constraints(constraints(oi)))(ctx.toNewId)

                IR(value = ctx.toNewId(oi),
                   present = ctx.record(internalMap(ctx)(prez)),
                   valid = ctx.record(internalMap(ctx)(validity)))
            }
          }

        })
        .fixID
    val optimized = finalTree.transform(SatisfactionProblem.Optimizations.optimizer).fixID
    LazyTree(optimized)(_noLambdas.root).forceEvaluation
  }
  sealed trait Node[I]
  object Node {
    implicit object Functor extends SFunctor[Node] {
      override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: Node[A])(f: A => B): Node[B] = {
        fa match {
          case Constraints(t) => Constraints(t.map { case (si, i) => (si.map(f), f(i)) })
          case Tot(t)         => Tot(t.smap(f))
          case Prezence(t)    => Prezence(t.map(f))
        }
      }
    }
  }
  final case class Tot[I](t: Total[I]) extends Node[I]
  final case class Prezence[I](t: Set[I]) extends Node[I]
  final case class Constraints[I](t: Seq[(Set[I], I)]) extends Node[I]
}
