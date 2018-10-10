package dahu.model.problem

import cats.Functor
import cats.implicits._
import dahu.graphs.TreeNode
import dahu.graphs.TreeNode._
import dahu.model.functions.{Box, Fun, Reversible, Unbox}
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

    def isBox(f: Fun[_]): Boolean = f match {
      case fr: Reversible[_, _] if fr.name == "box" => true
      case _                                        => false
    }
    def isUnbox(f: Fun[_]): Boolean = f match {
      case fr: Reversible[_, _] if fr.name == "unbox" => true
      case _                                          => false
    }

    import syntax._
    trait OptimizationContext {
      def retrieve(i: SomeID): Total[SomeID]
      def record(fi: Total[SomeID]): SomeID
      lazy val TRUE: SomeID = record(bool.TrueF)
      lazy val FALSE: SomeID = record(bool.FalseF)
    }

    abstract class Optimizer2(ctx: OptimizationContext) extends (Total[SomeID] => Total[SomeID]) {
      def retrieve(i: SomeID) = ctx.retrieve(i)
      def record(fi: Total[SomeID]): SomeID = {
        if(current != null && current == fi)
          throw RecursiveTransformation
        else {
          ctx.record(fi)
        }
      }

      protected def optimImpl(fi: Total[SomeID]): Total[SomeID]

      private var current: Total[SomeID] = null
      def apply(fi: Total[SomeID]): Total[SomeID] = {
        current = fi
        try {
          optimImpl(fi)
        } catch {
          case RecursiveTransformation => fi
        }
      }
    }

    final class ElimUniversalEquality(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
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
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
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

    final class DistributeBoxIfThenElse(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
        case orig @ ComputationF(f, Vec(a), t) if isBox(f) || isUnbox(f) =>
          retrieve(a) match {
            case ITEF(cond, onTrue, onFalse, typ) =>
              val trueCase = record(ComputationF(f, Vec(onTrue), t))
              val falseCase = record(ComputationF(f, Vec(onFalse), t))
              ITEF(cond, trueCase, falseCase, t)
            case _ => orig
          }
        case x => x
      }
    }

    final class SimplifyIfThenElse(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
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
      def dom(t: TagAny): Interval[Int] = t match {
        case t: TagIsoInt[_] => Interval(t.min, t.max)
        case _               => Interval.all
      }
      def dom(e: Total[SomeID]): Interval[Int] = e match {
        case ComputationF(f: Unbox[_], _, _) =>
          dom(f.inType) intersect dom(f.outType)
        case CstF(v: Int, _) =>
          Interval.point(v)
        case x =>
          dom(x.typ)
      }
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
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
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
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
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
        case x @ ComputationF(f: Monoid[_], args, _) if args.isEmpty => f.liftedIdentity
        case x @ ComputationF(_: Monoid[_], Vec(arg), _)             => retrieve(arg)
        case x                                                       => x
      }
    }

    final class ElimDuplicationsIdempotentMonoids(ctx: OptimizationContext)
        extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
        case ComputationF(f: IdempotentMonoid[_], args, tpe)
            if f.isInstanceOf[CommutativeMonoid[_]] =>
          ComputationF(f, args.distinct, tpe)
        case x => x
      }
    }

    final class FlattenMonoids(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
        case x @ ComputationF(f: Monoid[_], args, tpe) =>
          val buff = debox.Buffer[SomeID]()
          cfor(0)(_ < args.length, _ + 1) { i =>
            retrieve(args(i)) match {
              case ComputationF(f2: Monoid[_], args2, _) if f2 == f =>
                args2.foreach(buff += _)
              case _ =>
                buff += args(i)
            }
          }
          if(buff.length != args.size)
            ComputationF[SomeID](f, Vec.unsafe(buff.toArray()), tpe)
          else
            x
        case x => x
      }
    }

    final class ConstantFolding(ctx: OptimizationContext) extends Optimizer2(ctx) {
      import ctx.{FALSE, TRUE}

      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
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
          val buff = debox.Buffer[SomeID]()
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
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
        case x @ ComputationF(f: CommutativeMonoid[_], args, tpe) =>
          if(args.isSorted)
            x
          else
            ComputationF(f, args.sorted, tpe)
        case x => x
      }
    }

    final class ElimNoops(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
        case NoopF(e, _) => ctx.retrieve(e)
        case _           => fi
      }
    }

    final class ExtractField(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
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
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
        case original @ ComputationF(bool.Or, Vec(a, b), _) =>
          def tryWith(a: SomeID, b: SomeID): Option[Total[SomeID]] =
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
      def not(a: SomeID): SomeID = {
        retrieve(a) match {
          case Not(na) => na
          case _       => record(Not(a))
        }
      }

      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
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
    final class GroupImplications(ctx: OptimizationContext) extends Optimizer2(ctx) {
      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
        case original @ And(as @ _*) =>
          val implications = mutable.ArrayBuffer[(SomeID, SomeID)]()
          val others = mutable.ArrayBuffer[SomeID]()
          for(a <- as) {
            retrieve(a) match {
              case Or(x, y) =>
                (retrieve(x), retrieve(y)) match {
                  case (Not(lhs), _) => implications += ((lhs, y))
                  case (_, Not(rhs)) => implications += ((rhs, x))
                  case _             => others += a
                }
              case _ => others += a
            }

          }
          val gp = implications.groupBy(_._1).toSeq.sortBy(t => retrieve(t._1).toString)
          val implictationConjuncts = gp.map {
            case (lhs, rhss) =>
              record(Or(lhs, record(And(rhss.map(_._2)))))
          }
          And(others.sortBy(retrieve(_).toString) ++ implictationConjuncts)
        case x => x
      }
    }

    final class SimplifyImplications(ctx: OptimizationContext) extends Optimizer2(ctx) {
      import ctx.{FALSE, TRUE}
      class ImplicationAccumulator(record: Total[SomeID] => SomeID) {
        private val conds = mutable.ArrayBuffer[Vec[SomeID]]()
        private val effs = mutable.ArrayBuffer[debox.Set[SomeID]]()
        def isForced(i: SomeID): Boolean = {
          if(conds.isEmpty || conds(0).size > 0)
            false
          else
            effs(0)(i)
        }
        def add(condition: Vec[SomeID], effects: Vec[SomeID]): Unit = {
          addToSame(condition, effects.filter(i => !isForced(i)))
        }
        def addToSame(condition: Vec[SomeID], effects: Vec[SomeID]): Unit = {
          val loc = conds.indexOf(condition)

          if(loc == -1) {
            conds += condition
            effs += debox.Set.fromArray(effects.toArray)
          } else {
            effs(loc).addAll(effects.toArray)
          }
        }
        def makeAnd(effs: Iterable[SomeID]): SomeID = {
          if(effs.isEmpty)
            TRUE
          else if(effs.size == 1)
            effs.head
          else
            record(And(effs))
        }
        def makeNot(conds: Vec[SomeID]): SomeID = {
          if(conds.size == 0)
            FALSE
          else if(conds.size == 1)
            record(Not(conds(0)))
          else
            record(Not(makeAnd(conds.toIterable)))
        }
        def makeOr(a: SomeID, b: SomeID): SomeID =
          if(a == TRUE || b == TRUE)
            TRUE
          else if(a == FALSE)
            b
          else if(b == FALSE)
            a
          else
            record(Or(a, b))
        def makeimplication(condition: Vec[SomeID], effs: debox.Set[SomeID]): SomeID = {
          makeOr(makeNot(condition), makeAnd(effs.toIterable()))
        }
        def compile(): Total[SomeID] = {
          val l = conds.length
          val conjuncts = new Array[SomeID](l)
          spire.syntax.cfor.cfor(0)(_ < conds.length, _ + 1) { i =>
            conjuncts(i) = makeimplication(conds(i), effs(i))
          }
          And(conjuncts.toIterable)
        }
      }

      override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
        case original @ And(args @ _*) =>
          def notConj(i: SomeID): Option[Vec[SomeID]] =
            retrieve(i) match {
              case Not(a) =>
                retrieve(a) match {
                  case And(notConj @ _*) => Some(notConj.toVec)
                  case _                 => Some(Vec(a))
                }
              case _ => None
            }
          def anyConj(i: SomeID): Vec[SomeID] =
            retrieve(i) match {
              case ComputationF(bool.And, args, _) => args
              case _                               => Vec(i)
            }
          def asImplication(i: SomeID): (Vec[SomeID], Vec[SomeID]) = {
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
        override def retrieve(i: SomeID): Total[SomeID] = ctx.retrieve(i)
        override def record(fi: Total[SomeID]): SomeID = ctx.record(self.apply(fi))
      }
      private val subOptimizers = subs.map(_.apply(subOptimizationContext)).toBuffer

      override protected def optimImpl(fi: Total[SomeID]): Total[SomeID] = {
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
      ctx => new DistributeBoxIfThenElse(ctx),
      ctx => new SimplifyIfThenElse(ctx),
      ctx => new DistributeImplication(ctx),
      ctx => new DistributeNot(ctx),
//      ctx => new SimplifyImplications(ctx),
//      ctx => new DistributeImplication(ctx),
      ctx => new OrderArgs(ctx),
    )

    def optimizer(_retrieve: SomeID => Total[SomeID],
                  _record: Total[SomeID] => SomeID): Total[SomeID] => Total[SomeID] =
      new RecursiveOptimizerCombinator(
        optimizers ++ optimizers,
        new OptimizationContext {
          override def retrieve(i: SomeID): Total[SomeID] = _retrieve(i)
          override def record(fi: Total[SomeID]): SomeID = _record(fi)
        }
      )

    def implicationGrouper(_retrieve: SomeID => Total[SomeID],
                           _record: Total[SomeID] => SomeID): Total[SomeID] => Total[SomeID] =
      new GroupImplications(new OptimizationContext {
        override def retrieve(i: SomeID): Total[SomeID] = _retrieve(i)
        override def record(fi: Total[SomeID]): SomeID = _record(fi)
      })
  }

}
