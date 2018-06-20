package dahu.model.problem

import cats.Functor
import cats.implicits._
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.model.functions.{Box, Reversible, Unbox}
import dahu.utils._
import dahu.model.ir._
import dahu.model.math._
import dahu.model.products.FieldAccess
import dahu.model.types._
import dahu.recursion._
import dahu.utils.errors._
import spire.math.Interval
import spire.syntax.cfor._

import scala.collection.{immutable, mutable}

object SatisfactionProblem {

  // TODO: move optimizations to distinct package
  object Optimizations {
    import syntax._
    trait OptimizationContext {
      def retrieve(i: IDTop): Total[IDTop]
      def record(fi: Total[IDTop]): IDTop
      lazy val TRUE: IDTop = record(bool.TrueF)
      lazy val FALSE: IDTop = record(bool.FalseF)
    }

    abstract class Optimizer2(ctx: OptimizationContext) extends (Total[IDTop] => Total[IDTop]) {
      def retrieve(i: IDTop) = ctx.retrieve(i)
      def record(fi: Total[IDTop]): IDTop = {
        if(current != null && current == fi)
          throw RecursiveTransformation
        else {
          ctx.record(fi)
        }
      }

      protected def optimImpl(fi: Total[IDTop]): Total[IDTop]

      private var current: Total[IDTop] = null
      def apply(fi: Total[IDTop]): Total[IDTop] = {
        current = fi
        try {
          optimImpl(fi)
        } catch {
          case RecursiveTransformation => fi
        }
      }
    }

    final class ElimUniversalEquality(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case orig @ ComputationF(f: any.EQ, Vec(x, y), _) if x == y => bool.TrueF
        case orig @ ComputationF(f: any.EQ, Vec(x, y), _) =>
          val fx = retrieve(x)
          val fy = retrieve(y)
          (fx, fy) match {
            case (SequenceF(xs, xt), SequenceF(ys, yt)) =>
//              require(xt == yt) // TODO: we should be able to test type equality for derived types
              if(xs.size != ys.size)
                bool.FalseF
              else {
                val pushedDown = xs.indices.map(i => {
                  val a = xs(i)
                  val b = ys(i)
                  record(ComputationF(any.EQ, Vec(a, b), Tag.ofBoolean))
                })
                ComputationF(bool.And, pushedDown, Tag.ofBoolean)
              }
            case (ProductF(xs, xt), ProductF(ys, yt)) =>
//              require(xt == yt)
              require(xs.size == ys.size)
              val pushedDown = xs.indices.map(i => {
                val a = xs(i)
                val b = ys(i)
                record(ComputationF(any.EQ, Vec(a, b), Tag.ofBoolean))
              })
              ComputationF(bool.And, pushedDown, Tag.ofBoolean)
            case (CstF(a, _), CstF(b, _)) =>
              if(a == b) bool.TrueF else bool.FalseF

            case _ if fx.typ == Tag.ofInt && fy.typ == Tag.ofInt =>
              ComputationF(int.EQ, Vec(x, y), Tag.ofBoolean)
            case _ =>
              (fx.typ, fy.typ) match {
                case (t1: TagIsoInt[_], t2: TagIsoInt[_]) =>
                  val ix = record(ComputationF(t1.unbox, Vec(x), t1.unbox.outType))
                  val iy = record(ComputationF(t2.unbox, Vec(y), t1.unbox.outType))
                  ComputationF(int.EQ, Vec(ix, iy), int.EQ.outType)
                case _ =>
//                  dahu.utils.debug.warning(s"Universal equality not specialized: $fx == $fy")
                  orig
              }

          }

        case x =>
          x
      }
    }

    final class ElimReversible(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case orig @ ComputationF(f: Reversible[_, _], Vec(arg), _) =>
          retrieve(arg) match {
            case ComputationF(f2: Reversible[_, _], Vec(arg2), _) if f2 == f.reverse =>
              retrieve(arg2)
            case ComputationF(f2: Box[_], _, _) => ???
            case _                              => orig
          }
        case x => x
      }
    }

    final class SimplifyIfThenElse(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case ITEF(_, onTrue, onFalse, _) if onTrue == onFalse => retrieve(onTrue)
        case orig @ ITEF(cond, onTrue, onFalse, t) if t == Tag.ofBoolean =>
          if(retrieve(onFalse) == bool.FalseF)
            ComputationF(bool.And, Vec(cond, onTrue), Tag.ofBoolean)
          else if(retrieve(onFalse) == bool.TrueF) {
            val notCond = record(ComputationF(bool.Not, Vec(cond), Tag.ofBoolean))
            ComputationF(bool.Or, Vec(notCond, onTrue), Tag.ofBoolean)
          } else
            orig
        case x => x
      }
    }

    final class ElimTautologies(ctx: OptimizationContext) extends Optimizer2(ctx) {
      def dom(t: Tag[_]): Interval[Int] = t match {
        case t: TagIsoInt[_] => Interval(t.min, t.max)
        case _               => Interval.all
      }
      def dom(e: Total[IDTop]): Interval[Int] = e match {
        case ComputationF(f: Unbox[_], _, _) =>
          dom(f.inType) intersect dom(f.outType)
        case CstF(v: Int, _) =>
          Interval.point(v)
        case x =>
          dom(x.typ)
      }
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        // note: those two cases can be extended to vectors of arbitrary size
        case original @ ComputationF(bool.Or, Vec(a, b), _) =>
          if(b == record(ComputationF(bool.Not, Vec(a), Tag.ofBoolean)))
            bool.TrueF
          else if(a == record(ComputationF(bool.Not, Vec(b), Tag.ofBoolean)))
            bool.TrueF
          else
            original
        case original @ ComputationF(bool.And, Vec(a, b), _) =>
          if(b == record(ComputationF(bool.Not, Vec(a), Tag.ofBoolean)))
            bool.FalseF
          else if(a == record(ComputationF(bool.Not, Vec(b), Tag.ofBoolean)))
            bool.FalseF
          else
            original

        case ComputationF(int.EQ, Vec(a1, a2), _) if a1 == a2  => bool.TrueF
        case ComputationF(int.LEQ, Vec(a1, a2), _) if a1 == a2 => bool.TrueF
        case x @ ComputationF(int.EQ, Vec(a1, a2), _) =>
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

    final class ElimIdentity(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case x @ ComputationF(f: Monoid[_], args, tpe) =>
          val identity = record(f.liftedIdentity)
          if(args.contains(identity))
            ComputationF(f, args.filter(_ != identity), tpe)
          else
            x
        case x => x
      }
    }

    final class ElimEmptyAndSingletonMonoid(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case x @ ComputationF(f: Monoid[_], args, _) if args.isEmpty => f.liftedIdentity
        case x @ ComputationF(_: Monoid[_], Vec(arg), _)             => retrieve(arg)
        case x                                                       => x
      }
    }

    final class ElimDuplicationsIdempotentMonoids(ctx: OptimizationContext)
        extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case ComputationF(f: IdempotentMonoid[_], args, tpe)
            if f.isInstanceOf[CommutativeMonoid[_]] =>
          ComputationF(f, args.distinct, tpe)
        case x => x
      }
    }

    final class FlattenMonoids(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case x @ ComputationF(f: Monoid[_], args, tpe) =>
          val buff = debox.Buffer[IDTop]()
          cfor(0)(_ < args.length, _ + 1) { i =>
            retrieve(args(i)) match {
              case ComputationF(f2: Monoid[_], args2, _) if f2 == f =>
                args2.foreach(buff += _)
              case _ =>
                buff += args(i)
            }
          }
          if(buff.length != args.size)
            ComputationF[IDTop](f, Vec.unsafe(buff.toArray()), tpe)
          else
            x
        case x => x
      }
    }

    final class ConstantFolding(ctx: OptimizationContext) extends Optimizer2(ctx) {
      import ctx.{FALSE, TRUE}

      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
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
          val buff = debox.Buffer[IDTop]()
          var acc: Value = Value(f.identity)
          cfor(0)(_ < args.length, _ + 1) { i =>
            retrieve(args(i)) match {
              case CstF(value, _) => acc = f.combineUnsafe(acc, value)
              case _              => buff += args(i)
            }
          }
          buff += record(CstF(acc, f.outType))
          ComputationF(f, Vec.unsafe(buff.toArray()), tpe)

        case x @ ITEF(cond, onTrue, onFalse, _) =>
          if(cond == TRUE) retrieve(onTrue)
          else if(cond == FALSE) retrieve(onFalse)
          else x

        case x => x
      }
    }

    final class OrderArgs(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case x @ ComputationF(f: CommutativeMonoid[_], args, tpe) =>
          if(args.isSorted)
            x
          else
            ComputationF(f, args.sorted, tpe)
        case x => x
      }
    }

    final class ElimNoops(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case NoopF(e, _) => ctx.retrieve(e)
        case _           => fi
      }
    }

    final class ExtractField(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case x @ ComputationF(fa: FieldAccess[_, _], Vec(p), _) =>
          retrieve(p) match {
            case ProductF(members, _) =>
              retrieve(members(fa.fieldPosition))
            case unmatched =>
//              debug.warning(s"Field was not extracted: $fa of $unmatched")
              x
          }
        case ComputationF(fa: FieldAccess[_, _], _, _) => unexpected
        case x                                         => x
      }
    }

    final class DistributeImplication(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case original @ ComputationF(bool.Or, Vec(a, b), _) =>
          def tryWith(a: IDTop, b: IDTop): Option[Total[IDTop]] =
            (retrieve(a), retrieve(b)) match {
              case (fa @ ComputationF(bool.Not, _, _), ComputationF(bool.And, conjs, _)) =>
                val disjuncts =
                  conjs.map(c => record(ComputationF(bool.Or, Vec(a, c), Tag.ofBoolean)))
                Some(ComputationF(bool.And, disjuncts, Tag.ofBoolean))
              case _ => None
            }

          tryWith(a, b).orElse(tryWith(b, a)).getOrElse(original)
        case x => x
      }
    }
    final class DistributeNot(ctx: OptimizationContext) extends Optimizer2(ctx) {
      def not(a: IDTop): IDTop = {
        retrieve(a) match {
          case Not(na) => na
          case _       => record(Not(a))
        }
      }

      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case original @ Not(a) =>
          retrieve(a) match {
            case Or(disjs @ _*) =>
              val conjs =
                disjs.map(d => not(d))
              And(conjs)

            case And(conjs @ _*) =>
              val disjs =
                conjs.map(d => not(d))
              Or(disjs)
            case _ => original
          }
        case x => x
      }
    }

    final class SimplifyImplications(ctx: OptimizationContext) extends Optimizer2(ctx) {
      import ctx.{FALSE, TRUE}
      class ImplicationAccumulator(record: Total[IDTop] => IDTop) {
        private val conds = mutable.ArrayBuffer[Vec[IDTop]]()
        private val effs = mutable.ArrayBuffer[debox.Set[IDTop]]()
        def isForced(i: IDTop): Boolean = {
          if(conds.isEmpty || conds(0).size > 0)
            false
          else
            effs(0)(i)
        }
        def add(condition: Vec[IDTop], effects: Vec[IDTop]): Unit = {
          addToSame(condition, effects.filter(i => !isForced(i)))
        }
        def addToSame(condition: Vec[IDTop], effects: Vec[IDTop]): Unit = {
          val loc = conds.indexOf(condition)

          if(loc == -1) {
            conds += condition
            effs += debox.Set.fromArray(effects.toArray)
          } else {
            effs(loc).addAll(effects.toArray)
          }
        }
        def makeAnd(effs: Iterable[IDTop]): IDTop = {
          if(effs.isEmpty)
            TRUE
          else if(effs.size == 1)
            effs.head
          else
            record(And(effs))
        }
        def makeNot(conds: Vec[IDTop]): IDTop = {
          if(conds.size == 0)
            FALSE
          else if(conds.size == 1)
            record(Not(conds(0)))
          else
            record(Not(makeAnd(conds.toIterable)))
        }
        def makeOr(a: IDTop, b: IDTop): IDTop =
          if(a == TRUE || b == TRUE)
            TRUE
          else if(a == FALSE)
            b
          else if(b == FALSE)
            a
          else
            record(Or(a, b))
        def makeimplication(condition: Vec[IDTop], effs: debox.Set[IDTop]): IDTop = {
          makeOr(makeNot(condition), makeAnd(effs.toIterable()))
        }
        def compile(): Total[IDTop] = {
          val l = conds.length
          val conjuncts = new Array[IDTop](l)
          spire.syntax.cfor.cfor(0)(_ < conds.length, _ + 1) { i =>
            conjuncts(i) = makeimplication(conds(i), effs(i))
          }
          And(conjuncts.toIterable)
        }
      }

      override def optimImpl(fi: Total[IDTop]): Total[IDTop] = fi match {
        case original @ And(args @ _*) =>
          def notConj(i: IDTop): Option[Vec[IDTop]] =
            retrieve(i) match {
              case Not(a) =>
                retrieve(a) match {
                  case And(notConj @ _*) => Some(notConj.toVec)
                  case _                 => Some(Vec(a))
                }
              case _ => None
            }
          def anyConj(i: IDTop): Vec[IDTop] =
            retrieve(i) match {
              case ComputationF(bool.And, args, _) => args
              case _                               => Vec(i)
            }
          def asImplication(i: IDTop): (Vec[IDTop], Vec[IDTop]) = {
            retrieve(i) match {
              case Or(a, b) =>
                notConj(a) match {
                  case Some(conditions) =>
                    (conditions, anyConj(b))
                  case None =>
                    notConj(b) match {
                      case Some(condtions) => (condtions, anyConj(a))
                      case None            => (Vec.empty, Vec(i))
                    }
                }
              case _ => (Vec.empty, Vec(i))
            }
          }

          val implications = args.map(asImplication).sortBy(_._1.size)
          val acc = new ImplicationAccumulator(record)
          implications.foreach(t => acc.add(t._1, t._2))
          acc.compile()
        case x => x
      }
    }

    final class RecursiveOptimizerCombinator(subs: Seq[OptimizationContext => Optimizer2],
                                             ctx: OptimizationContext)
        extends Optimizer2(ctx) { self =>
      private val subOptimizationContext = new OptimizationContext {
        override def retrieve(i: IDTop): Total[IDTop] = ctx.retrieve(i)
        override def record(fi: Total[IDTop]): IDTop = ctx.record(self.apply(fi))
      }
      private val subOptimizers = subs.map(_.apply(subOptimizationContext)).toBuffer

      override protected def optimImpl(fi: Total[IDTop]): Total[IDTop] = {
        subOptimizers.foldLeft(fi)((fi2, opt) => opt(fi2))
      }
    }

    def optimizers: Seq[OptimizationContext => Optimizer2] = Seq(
      ctx => new ElimNoops(ctx),
      ctx => new ExtractField(ctx),
      ctx => new ElimUniversalEquality(ctx),
      ctx => new ElimReversible(ctx),
      ctx => new ElimIdentity(ctx),
      ctx => new ElimEmptyAndSingletonMonoid(ctx),
      ctx => new FlattenMonoids(ctx),
      ctx => new ElimDuplicationsIdempotentMonoids(ctx),
      ctx => new ElimTautologies(ctx),
      ctx => new ConstantFolding(ctx),
      ctx => new SimplifyIfThenElse(ctx),
      ctx => new DistributeImplication(ctx),
      ctx => new DistributeNot(ctx),
//      ctx => new SimplifyImplications(ctx),
//      ctx => new DistributeImplication(ctx),
      ctx => new OrderArgs(ctx),
    )

    def optimizer(_retrieve: IDTop => Total[IDTop],
                  _record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] =
      new RecursiveOptimizerCombinator(
        optimizers ++ optimizers,
        new OptimizationContext {
          override def retrieve(i: IDTop): Total[IDTop] = _retrieve(i)
          override def record(fi: Total[IDTop]): IDTop = _record(fi)
        }
      )
  }

  final class Utils(val directRec: Total[IDTop] => IDTop, retrieve: IDTop => Total[IDTop]) {
    private val optim = Optimizations.optimizer(retrieve, directRec)
    def rec(expr: Total[IDTop]): IDTop =
      directRec(optim(expr))
    def ret(i: IDTop): Total[IDTop] = retrieve(i)

    val TRUE: IDTop = rec(CstF(Value(true), Tag.ofBoolean))
    val FALSE: IDTop = rec(CstF(Value(false), Tag.ofBoolean))

    def and(conjuncts: IDTop*): IDTop = and(Vec.unsafe(conjuncts.toArray))
    def and(conjuncts: Vec[IDTop]): IDTop = rec(ComputationF(bool.And, conjuncts, Tag.ofBoolean))
    def or(disjuncts: IDTop*): IDTop =
      rec(ComputationF(bool.Or, Vec.unsafe(disjuncts.toArray), Tag.ofBoolean))
    def not(e: IDTop): IDTop =
      rec(ComputationF(bool.Not, Seq(e), Tag.ofBoolean))

    def implies(cond: IDTop, eff: IDTop): IDTop =
      or(not(cond), eff)

  }

  final case class IR[@specialized(Int) A](value: A, present: A, valid: A)
  object IR {
    implicit val functorInstance: Functor[IR] = new Functor[IR] {
      override def map[A, B](fa: IR[A])(f: A => B): IR[B] =
        IR(f(fa.value), f(fa.present), f(fa.valid))
    }

  }
  final case class TotalValue[F[_], A](value: F[A], present: F[Boolean], valid: F[Boolean])
  object TotalValue {
    type TV[F[_]] = TotalValue[F, Value]
    implicit val typeTag: Tag[Value] = Tag.default[Value]
    implicit val productTag: ProductTag[TV] = ProductTag.ofProd[TV]

    val Value: FieldAccess[TV, Value] = FieldAccess[TV, Value]("value", 0)
    val Present: FieldAccess[TV, Boolean] = FieldAccess[TV, Boolean]("present", 1)
    val Valid: FieldAccess[TV, Boolean] = FieldAccess[TV, Boolean]("valid", 2)
  }

  def compiler(
      context: LazyForestGenerator.Context[Total, IDTop]): StaticF[IR[IDTop]] => IR[IDTop] =
    x => {
      val utils = new Utils(context.record, context.retrieve)
      val ir = x match {
        case x: InputF[_] => IR(utils.rec(x), utils.TRUE, utils.TRUE)
        case x: CstF[_]   => IR(utils.rec(x), utils.TRUE, utils.TRUE)
        case ComputationF(f, args, t) =>
          IR(
            value = utils.rec(ComputationF(f, args.map(a => a.value), t)),
            present = utils.and(args.map(_.present)),
            valid = utils.and(args.map(_.valid))
          )
        case ProductF(members, t) =>
          IR(
            value = utils.rec(ProductF(members.map(a => a.value), t)),
            present = utils.and(members.map(_.present)),
            valid = utils.and(members.map(_.valid))
          )
        case SequenceF(members, t) =>
          IR(
            value = utils.rec(SequenceF(members.map(a => a.value), t)),
            present = utils.and(members.map(_.present)),
            valid = utils.and(members.map(_.valid))
          )
        case ITEF(cond, onTrue, onFalse, t) =>
          IR(
            value = utils.rec(ITEF(cond.value, onTrue.value, onFalse.value, t)),
            present = utils.and(cond.present,
                                utils.implies(cond.value, onTrue.present),
                                utils.implies(utils.not(cond.value), onFalse.present)),
            valid = utils.and(cond.valid,
                              utils.implies(cond.value, onTrue.valid),
                              utils.implies(utils.not(cond.value), onFalse.valid))
          )
        case OptionalF(value, present, _) =>
          import utils._
          def isReallyValid(ir: IR[IDTop]) = utils.implies(ir.present, ir.valid)
          val validity =
            utils.and(utils.or(utils.not(present.value), isReallyValid(value)),
                      isReallyValid(present))
          val presence =
            utils.or(utils.and(value.present, present.present, present.value), utils.not(validity))
//          val p2 = utils.or(utils.and(present.present, present.value, value.present),
//                            utils.not(present.valid))
          val absent =
            or(not(value.present), not(present.present), and(not(present.value), present.valid))
          val p2 = not(absent)
          val val2 = utils.implies(p2, utils.and(value.valid, present.valid))
          IR(
            value = value.value,
            present = p2,
            valid = val2
          )
        case PresentF(opt) =>
          IR(
            value = opt.present,
            present = utils.TRUE,
            valid = opt.valid
          )
        case ValidF(part) =>
          IR(
            value = part.valid,
            present = part.present,
            valid = utils.TRUE
          )
        case Partial(value, condition, tpe) =>
          val prez = utils.and(value.present, condition.present)
          val valid = utils.implies(prez, utils.and(value.valid, condition.valid, condition.value))
          IR(
            value = value.value,
            present = prez, //value.present,
            valid = valid
//              utils.and(
//              utils.implies(value.present, value.valid),
//              utils.implies(condition.present, utils.and(condition.value, condition.valid)))
          )
        case p @ LambdaParamF(id, typ) =>
          val placeHolder = utils.rec(LambdaParamF(id, typ))
          IR(
            value = utils.rec(ComputationF(TotalValue.Value, Vec(placeHolder), typ)),
            present = utils.rec(ComputationF(TotalValue.Present, Vec(placeHolder), Tag.ofBoolean)),
            valid = utils.rec(ComputationF(TotalValue.Valid, Vec(placeHolder), Tag.ofBoolean))
          )
        case LambdaF(in, tree, id, tpe) =>
          val presentValidTag = LambdaTag.LambdaTagImpl(tpe, Tag.ofBoolean)
          IR(
            value = utils.rec(LambdaF(in.value, tree.value, id, tpe)),
            present = utils.rec(LambdaF(in.value, tree.present, id, presentValidTag)),
            valid = utils.rec(LambdaF(in.value, tree.valid, id, presentValidTag))
          )
      }
      ir
    }

  // TODO: the StaticF is to wide as this method does not handle lambdaApplication
  def encode[X <: Int](root: X,
                       coalgebra: FCoalgebra[StaticF, X],
                       optimize: Boolean = true): LazyTree[X, Total, IR, _] = {
    val lt = IlazyForest.build(coalgebra)(compiler).fixID

    LazyTree(lt)(root)
  }

  def encode[X](_t: LazyTree[X, NoApplyF, cats.Id, _]): LazyTree[X, Total, IR, _] = {
    val t = _t.fixID
    val innerRoot = t.tree.getTreeRoot(t.root)
    val total =
      encode[t.ID](innerRoot, (k: t.ID) => t.tree.internalCoalgebra(k)).tree.fixID
        .changedKey[X](k2 => t.tree.getTreeRoot(k2))
    LazyTree(total)(t.root)
  }

  def encodePresence[K](root: K,
                        coalgebra: FCoalgebra[NoApplyF, K],
                        optimize: Boolean = true): LazyTree[K, OptConst, cats.Id, _] = {
    val _lt = IlazyForest.build[K, NoApplyF, OptConst, cats.Id](coalgebra)(compilerPresence)
    val lt = _lt.fixID
    LazyTree[K, OptConst, cats.Id, lt.ID](lt)(root)
  }

  final class UtilsPrez(val directRec: OptConst[IDTop] => IDTop,
                        retrieve: IDTop => OptConst[IDTop]) {
    def rec(expr: OptConst[IDTop]): IDTop = directRec(expr)
    def rec(e: ConstrainedF[IDTop], prez: IDTop): IDTop = rec(OptConst(e, Some(prez)))
    def recPure(expr: ConstrainedF[IDTop]) = rec(OptConst.present(expr))
    def ret(i: IDTop): OptConst[IDTop] = retrieve(i)

    val TRUE: IDTop = rec(OptConst[IDTop](CstF(Value(true), Tag.ofBoolean), None))
    val FALSE: IDTop = rec(OptConst[IDTop](CstF(Value(false), Tag.ofBoolean), None))

    def and(conjuncts: IDTop*): IDTop = andVec(Vec.unsafe(conjuncts.toArray))
    def andVec(conjuncts: Vec[IDTop]): IDTop = {
      conjuncts.filter(_ != TRUE) match {
        case Vec()  => TRUE
        case Vec(a) => a
        case args   => recPure(ComputationF(bool.And, args, Tag.ofBoolean))
      }
    }
    def presence(i: IDTop): IDTop = ret(i).presence.getOrElse(TRUE)
    def or(disjuncts: IDTop*): IDTop =
      recPure(ComputationF(bool.Or, Vec.unsafe(disjuncts.toArray), Tag.ofBoolean))
    def not(e: IDTop): IDTop =
      recPure(ComputationF(bool.Not, Vec(e), Tag.ofBoolean))

    def implies(cond: IDTop, eff: IDTop): IDTop =
      or(not(cond), eff)

  }

  final case class Prez[@sp(Int) F](value: F, present: F)
  object Prez {
    implicit object functorInstance extends Functor[Prez] {
      override def map[A, B](fa: Prez[A])(f: A => B): Prez[B] = Prez(f(fa.value), f(fa.present))
    }
  }
  final case class OptConst[F](value: ConstrainedF[F], presence: Option[F])
  object OptConst {
    def present[F](value: ConstrainedF[F]): OptConst[F] = OptConst(value, None)
    def apply[F](value: ConstrainedF[F], prez: F): OptConst[F] = OptConst(value, Some(prez))

    implicit object FunctorInstance extends SFunctor[OptConst] {
      override def smap[@sp(Int) A, @sp(Int) B: ClassTag](fa: OptConst[A])(f: A => B): OptConst[B] =
        OptConst(fa.value.smap(f), fa.presence.map(f))
    }

    implicit object TreeNodeInstance extends TreeNode[OptConst] {
      override def children[K](n: OptConst[K]): immutable.Iterable[K] =
        n.value.children ++ n.presence.toIterable
    }
  }
  def compilerPresence(context: LazyForestGenerator.Context[OptConst, IDTop])
    : StaticF[cats.Id[IDTop]] => cats.Id[IDTop] =
    x => {
      val utils = new UtilsPrez(context.record, context.retrieve)
      import utils._

      def allPresent(es: Vec[IDTop]): IDTop =
        andVec(es.map(presence)) //.toList.flatMap(a => utils.ret(a).presence.toList): _*)
      val ir: IDTop = x match {
        case x: InputF[_] => recPure(x)
        case x: CstF[_]   => recPure(x)
        case x @ ComputationF(f, args, t) =>
          rec(x, allPresent(args))
        case x @ ProductF(members, t) =>
          rec(x, allPresent(members))
        case x @ SequenceF(members, t) =>
          rec(x, allPresent(members))
        case x @ ITEF(cond, onTrue, onFalse, t) =>
          val present = and(presence(cond),
                            implies(cond, presence(onTrue)),
                            implies(not(cond), presence(onFalse)))
          rec(x, present)
//          Prez(
//            value = utils.rec(ITEF(cond.value, onTrue.value, onFalse.value, t)),
//            present = utils.and(cond.present,
//                                utils.implies(cond.value, onTrue.present),
//                                utils.implies(utils.not(cond.value), onFalse.present))
//          )
        case OptionalF(value, present, tpe) =>
          rec(NoopF(value, tpe), and(present, presence(value)))
//          Prez(
//            value = value.value,
//            present = utils.and(value.present, present.present, present.value)
//          )
        case PresentF(opt) =>
          presence(opt)
        case x @ ValidF(part) => rec(x, presence(part))
//          Prez(
//            value = utils.rec(ValidF(part.value)),
//            present = part.present
//          )
        case x @ Partial(value, condition, tpe) => // TODO: are we present even if condition is absent?
          rec(x, presence(value))
//          Prez(
//            value = utils.rec(Partial(value.value, condition.value, tpe)),
//            present = utils.and(value.present, condition.present)
//          )
        case p @ LambdaParamF(id, typ) => ???
//          val placeHolder = utils.rec(LambdaParamF(id, typ))
//          Prez(
//            value = utils.rec(ComputationF(TotalValue.Value, Vec(placeHolder), typ)),
//            present = utils.rec(ComputationF(TotalValue.Present, Vec(placeHolder), Tag.ofBoolean))
//          )
        case LambdaF(in, tree, id, tpe) => ???
//          val presentValidTag = LambdaTag.LambdaTagImpl(tpe, Tag.ofBoolean)
//          Prez(
//            value = utils.rec(LambdaF(in.value, tree.value, id, tpe)),
//            present = utils.rec(LambdaF(in.value, tree.present, id, presentValidTag))
//          )
        case NoopF(expr, _) =>
          expr
//          Prez(
//            value = expr.value,
//            present = expr.present
//          )
      }
      ir
    }

}
