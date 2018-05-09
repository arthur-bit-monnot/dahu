package dahu.model.problem

import cats.Functor
import cats.implicits._
import dahu.model.compiler.Algebras
import dahu.model.functions.{Box, Reversible, Unbox}
import dahu.utils._
import dahu.model.ir._
import dahu.model.math._
import dahu.model.types._
import dahu.recursion._
import dahu.utils.SFunctor
import dahu.utils.Vec._
import dahu.utils.errors._
import spire.math.Interval
import spire.syntax.cfor._

import scala.collection.mutable
import scala.reflect.ClassTag

object SatisfactionProblem {

  def satisfactionSubAST(ast: AST[_]): RootedLazyTree[ast.ID, Total, cats.Id] = {
    encode(ast.root, ast.tree.asFunction)
  }
  type ID = Int
  object Optimizations {
    trait Optimizer {
      self =>
      def optim(retrieve: ID => Total[ID], record: Total[ID] => ID): Total[ID] => Total[ID]

      def andThen(next: Optimizer): Optimizer = new Optimizer {
        override def optim(retrieve: ID => Total[ID],
                           record: Total[ID] => ID): Total[ID] => Total[ID] =
          self.optim(retrieve, record) andThen next.optim(retrieve, record)

        override def toString: String = s"self >> next"
      }
    }

    object NoOpOptimizer extends Optimizer {
      override def optim(retrieve: ID => Total[ID],
                         record: Total[ID] => ID): Total[ID] => Total[ID] = identity[Total[ID]]
    }

    object ElimReversible extends Optimizer {
      override def optim(retrieve: ID => Total[ID],
                         record: Total[ID] => ID): Total[ID] => Total[ID] = {
        case orig @ ComputationF(f: Reversible[_, _], Vec1(arg), _) =>
          retrieve(arg) match {
            case ComputationF(f2: Reversible[_, _], Vec1(arg2), _) if f2 == f.reverse =>
              retrieve(arg2)
            case ComputationF(f2: Box[_], _, _) =>
              println(f)
              println(f2)
              ???
            case _ => orig
          }
        case x => x
      }
    }

    object ElimTautologies extends Optimizer {
      def dom(t: Tag[_]): Interval[Int] = t match {
        case t: TagIsoInt[_] => Interval(t.min, t.max)
        case _               => Interval.all
      }
      def dom(e: Total[ID]): Interval[Int] = e match {
        case ComputationF(f: Unbox[_], _, _) =>
          dom(f.inType) intersect dom(f.outType)
        case CstF(v: Int, _) =>
          Interval.point(v)
        case x =>
          dom(x.typ)
      }
      override def optim(retrieve: ID => Total[ID],
                         record: Total[ID] => ID): Total[ID] => Total[ID] = {
        case ComputationF(int.EQ, Vec2(a1, a2), _) if a1 == a2  => bool.TrueF
        case ComputationF(int.LEQ, Vec2(a1, a2), _) if a1 == a2 => bool.TrueF
        case x @ ComputationF(int.EQ, Vec2(a1, a2), _) =>
          val dom1 = dom(retrieve(a1))
          val dom2 = dom(retrieve(a2))
          if(!(dom1 intersects dom1))
            bool.FalseF
          else if(dom1.isPoint && dom1 == dom2)
            bool.TrueF
          else
            x

        case x => x
      }

    }

    object ElimIdentity extends Optimizer {
      override def optim(retrieve: ID => Total[ID],
                         record: Total[ID] => ID): Total[ID] => Total[ID] = {
        case x @ ComputationF(f: Monoid[_], args, tpe) =>
          val identity = record(f.liftedIdentity)
          if(args.contains(identity))
            ComputationF(f, args.filter(_ != identity), tpe)
          else
            x
        case x => x
      }
    }

    object ElimEmptyAndSingletonMonoid extends Optimizer {
      override def optim(retrieve: ID => Total[ID],
                         record: Total[ID] => ID): Total[ID] => Total[ID] = {
        case x @ ComputationF(f: Monoid[_], args, _) if args.isEmpty => f.liftedIdentity
        case x @ ComputationF(_: Monoid[_], Vec1(arg), _)            => retrieve(arg)
        case x                                                       => x
      }
    }

    object ElimDuplicationsIdempotentMonoids extends Optimizer {
      override def optim(retrieve: ID => Total[ID],
                         record: Total[ID] => ID): Total[ID] => Total[ID] = {
        case ComputationF(f: IdempotentMonoid[_], args, tpe)
            if f.isInstanceOf[CommutativeMonoid[_]] =>
          ComputationF(f, args.distinct, tpe)
        case x => x
      }
    }

    object FlattenMonoids extends Optimizer {
      override def optim(retrieve: ID => Total[ID],
                         record: Total[ID] => ID): Total[ID] => Total[ID] = {
        case x @ ComputationF(f: Monoid[_], args, tpe) =>
          val buff = debox.Buffer[ID]()
          cfor(0)(_ < args.length, _ + 1) { i =>
            retrieve(args(i)) match {
              case ComputationF(f2: Monoid[_], args2, _) if f2 == f =>
                args2.foreach(buff += _)
              case _ =>
                buff += args(i)
            }
          }
          if(buff.length != args.size)
            ComputationF[ID](f, Vec.unsafe(buff.toArray()), tpe)
          else
            x
        case x => x
      }
    }

    object ConstantFolding extends Optimizer {
      override def optim(retrieve: ID => Total[ID],
                         record: Total[ID] => ID): Total[ID] => Total[ID] = o => {
        val TRUE: ID = record(bool.TrueF)
        val FALSE: ID = record(bool.FalseF)

        o match {
          case ComputationF(bool.And, args, _) if args.contains(FALSE) => retrieve(FALSE)
          case ComputationF(bool.Or, args, _) if args.contains(TRUE)   => retrieve(TRUE)
          // commutative monoid, evaluate the combination of all constants args

          // any function, evaluate if all args are constant
          case ComputationF(f, args, t) if args.forall(retrieve(_).isInstanceOf[CstF[_]]) =>
            val params = args.map(i =>
              retrieve(i) match {
                case CstF(value, _) => value
                case _              => unexpected
            })
            CstF(Value(f.compute(params)), t)

          case ComputationF(f: CommutativeMonoid[_], args, tpe)
              if args.count(retrieve(_).isInstanceOf[CstF[_]]) >= 2 =>
            val buff = debox.Buffer[ID]()
            var acc: Value = Value(f.identity)
            cfor(0)(_ < args.length, _ + 1) { i =>
              retrieve(args(i)) match {
                case CstF(value, _) => acc = f.combineUnsafe(acc, value)
                case _              => buff += args(i)
              }
            }
            buff += record(CstF(acc, f.outType))
            ComputationF(f, Vec.unsafe(buff.toArray()), tpe)

          case x => x
        }
      }
    }

    object OrderArgs extends Optimizer {
      override def optim(retrieve: ID => Total[ID],
                         record: Total[ID] => ID): Total[ID] => Total[ID] = {
        case x @ ComputationF(f: CommutativeMonoid[_], args, tpe) =>
          if(args.isSorted)
            x
          else
            ComputationF(f, args.sorted, tpe)
        case x => x
      }
    }
    private val optimizers = List(
      ElimReversible,
      ElimIdentity,
      ElimEmptyAndSingletonMonoid,
      FlattenMonoids,
      ElimDuplicationsIdempotentMonoids,
      ElimTautologies,
      ConstantFolding,
      OrderArgs,
    )
    val optimizer: Optimizer = (optimizers ++ optimizers).foldLeft[Optimizer](NoOpOptimizer) {
      case (acc, next) => acc.andThen(next)
    }
  }

  trait TreeNode[N[_]] {
    def children[K](n: N[K]): Iterable[K]
  }
  object TreeNode {
    implicit val totalInstance: TreeNode[Total] = new TreeNode[Total] {
      override def children[A](fa: Total[A]): Iterable[A] = fa match {
        case ComputationF(_, args, _) => args.toSeq
        case _: CstF[A]               => Nil
        case _: InputF[A]             => Nil
        case ITEF(c, t, f, _)         => Seq(c, t, f)
        case ProductF(as, _)          => as.toSeq
      }
    }

    implicit val exprInstance: TreeNode[ExprF] = new TreeNode[ExprF] {
      override def children[A](fa: ExprF[A]): Iterable[A] = fa match {
        case Partial(value, condition, typ) => Seq(value, condition)
        case OptionalF(value, present, typ) => Seq(value, present)
        case PresentF(v)                    => Seq(v)
        case ValidF(v)                      => Seq(v)
        case x: Total[A]                    => totalInstance.children(x)
      }
    }
  }

  trait ILazyTree[K, F[_], Opt[_]] {
    type ID = Int
    def getExt(k: K): Opt[F[ID]]
    def getInternalID(k: K): Opt[ID]
    def getInt(i: ID): F[ID]

    def map[G[_]](f: F[Int] => G[Int])(implicit fOpt: Functor[Opt]): ILazyTree[K, G, Opt] =
      new MappedLazyTree(f, this)

    def build(id: ID)(implicit F: SFunctor[F], ct: ClassTag[F[Fix[F]]]): Fix[F] = {
      Recursion.ana(i => getInt(i))(id)
    }
  }

  class MappedLazyTree[K, F[_], G[_], Opt[_]: Functor](f: F[Int] => G[Int],
                                                       mapped: ILazyTree[K, F, Opt])
      extends ILazyTree[K, G, Opt] {
    private val memo = mutable.HashMap[ID, G[ID]]()

    override def getExt(k: K): Opt[G[ID]] = {
      getInternalID(k).map(getInt)
    }

    override def getInternalID(k: K): Opt[ID] = mapped.getInternalID(k)

    override def getInt(i: ID): G[ID] = memo.getOrElseUpdate(i, f(mapped.getInt(i)))
  }

  case class RootedLazyTree[K, F[_], Opt[_]: Functor](root: Int, tree: ILazyTree[K, F, Opt]) {
    def map[G[_]](f: F[Int] => G[Int]): RootedLazyTree[K, G, Opt] =
      RootedLazyTree(root, tree.map(f))

    def nodes(implicit tn: TreeNode[F]): Seq[(tree.ID, F[tree.ID])] = {
      val queue = mutable.Stack[tree.ID]()
      val visited = mutable.HashSet[tree.ID]()
      val result = mutable.ArrayBuffer[(tree.ID, F[tree.ID])]()
      queue.push(root)

      while(queue.nonEmpty) {
        val cur = queue.pop()
        val fcur = tree.getInt(cur)
        if(tn.children(fcur).forall(visited)) {
          visited += cur
          result += ((cur, fcur))
        } else {
          queue.push(cur)
          queue.pushAll(tn.children(fcur))
        }
      }
      result.toList
    }

    def fullTree(implicit F: SFunctor[F], ct: ClassTag[F[Fix[F]]]): Fix[F] = tree.build(root)
  }

  final class Context(val directRec: Total[ID] => ID, retrieve: ID => Total[ID]) {
    def rec(expr: Total[ID]): ID =
      directRec(Optimizations.optimizer.optim(retrieve, directRec)(expr))

    val TRUE: ID = rec(CstF(Value(true), Tag.ofBoolean))
    val FALSE: ID = rec(CstF(Value(false), Tag.ofBoolean))

    def and(conjuncts: ID*): ID = and(Vec.unsafe(conjuncts.toArray))
    def and(conjuncts: Vec[ID]): ID = rec(ComputationF(bool.And, conjuncts, Tag.ofBoolean))
    def or(disjuncts: ID*): ID =
      rec(ComputationF(bool.Or, Vec.unsafe(disjuncts.toArray), Tag.ofBoolean))
    def not(e: ID): ID =
      rec(ComputationF(bool.Not, Seq(e), Tag.ofBoolean))

    def implies(cond: ID, eff: ID): ID =
      or(not(cond), eff)

  }

  class LazyTreeSpec[@specialized(Int) K](f: K => ExprF[K], g: Context => ExprF[IR[ID]] => IR[ID]) {
    private val treeNode = implicitly[TreeNode[ExprF]]
    private val functor = implicitly[SFunctor[ExprF]]

    private val idsMap = mutable.HashMap[K, IR[ID]]()
    private val repMap = mutable.ArrayBuffer[Total[ID]]() // ID => Total[ID]
    private val memo = mutable.HashMap[Total[ID], ID]()

    private def getID(e: Total[ID]): ID = {
      if(memo.contains(e))
        memo(e)
      else {
        val id = repMap.size
        repMap += e
        memo += ((e, id))
        id
      }
    }
    private val ctx: Context = new Context(getID, get)

    private val g2: ExprF[IR[ID]] => IR[ID] = g(ctx)

    @inline private def processed(k: K): Boolean = idsMap.contains(k)

    def get(i: ID): Total[ID] = repMap(i)

    def get(key: K): IR[ID] = {
      val queue = mutable.Stack[K]()
      queue.push(key)

      while(queue.nonEmpty) {
        val cur = queue.pop()
        val fk = f(cur)
        if(treeNode.children(fk).forall(processed)) {
          val fg = functor.smap(fk)(id => idsMap(id))
          val g: IR[ID] = g2(fg)
          idsMap += ((cur, g))
        } else {
          queue.push(cur)
          queue.pushAll(treeNode.children(fk))
        }
      }
      idsMap(key)
    }
  }

  case class IR[@specialized(Int) A](value: A, present: A, valid: A)

  def compiler(ctx: Context): ExprF[IR[ID]] => IR[ID] = x => {
    val ir = x match {
      case x: InputF[_] => IR(ctx.rec(x), ctx.TRUE, ctx.TRUE)
      case x: CstF[_]   => IR(ctx.rec(x), ctx.TRUE, ctx.TRUE)
      case ComputationF(f, args, t) =>
        IR(
          value = ctx.rec(ComputationF(f, args.map(a => a.value), t)),
          present = ctx.and(args.map(_.present)),
          valid = ctx.and(args.map(_.valid))
        )
      case ProductF(members, t) =>
        IR(
          value = ctx.rec(ProductF(members.map(a => a.value), t)),
          present = ctx.and(members.map(_.present)),
          valid = ctx.and(members.map(_.valid))
        )
      case ITEF(cond, onTrue, onFalse, t) =>
        IR(
          value = ctx.rec(ITEF(cond.value, onTrue.value, onFalse.value, t)),
          present = ctx.and(cond.present,
                            ctx.implies(cond.value, onTrue.present),
                            ctx.implies(ctx.not(cond.value), onFalse.present)),
          valid = ctx.and(cond.valid,
                          ctx.implies(cond.value, onTrue.valid),
                          ctx.implies(ctx.not(cond.value), onFalse.valid))
        )
      case OptionalF(value, present, _) =>
        IR(
          value = value.value,
          present = ctx.and(value.present, present.present, present.value),
          valid = ctx.and(present.valid, value.valid)
        )
      case PresentF(opt) =>
        IR(
          value = opt.present,
          present = ctx.TRUE,
          valid = opt.valid
        )
      case ValidF(part) =>
        IR(
          value = part.valid,
          present = part.present,
          valid = ctx.TRUE
        )
      case Partial(value, condition, tpe) =>
        IR(
          value = value.value,
          present = value.present,
          valid = ctx.and(value.valid,
                          ctx.implies(condition.present, ctx.and(condition.value, condition.valid)))
        )
    }
    ir
  }

  def encode[@specialized(Int) X](root: X,
                                  coalgebra: FCoalgebra[ExprF, X],
                                  optimize: Boolean = true): RootedLazyTree[X, Total, cats.Id] = {
    val lt = new LazyTreeSpec[X](coalgebra, compiler)

    val totalTrees = new ILazyTree[X, Total, cats.Id] {
      override def getExt(k: X): Total[ID] = lt.get(lt.get(k).value)
      override def getInt(i: ID): Total[ID] = lt.get(i)
      override def getInternalID(k: X): ID = lt.get(k).value
    }
    val satRoot = lt.get(root).valid
    RootedLazyTree(satRoot, totalTrees)
  }

}
