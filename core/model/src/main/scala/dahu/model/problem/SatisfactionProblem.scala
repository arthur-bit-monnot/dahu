package dahu.model.problem

import cats.Functor
import cats.implicits._
import dahu.model.functions.{Box, Reversible, Unbox}
import dahu.model.input.{Ident, Input}
import dahu.utils._
import dahu.model.ir._
import dahu.model.math._
import dahu.model.products.FieldAccess
import dahu.model.types._
import dahu.recursion._
import dahu.utils.Vec._
import dahu.utils.errors._
import spire.math.Interval
import spire.syntax.cfor._

object SatisfactionProblem {

  // TODO: move optimizations to distinct package
  object Optimizations {
    trait Optimizer {
      self =>
      def optim(retrieve: IDTop => Total[IDTop],
                record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop]

      def andThen(next: Optimizer): Optimizer = new Optimizer {
        override def optim(retrieve: IDTop => Total[IDTop],
                           record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] =
          self.optim(retrieve, record) andThen next.optim(retrieve, record)

        override def toString: String = s"$self >> $next"
      }
    }

    object NoOpOptimizer extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] =
        identity[Total[IDTop]]
    }

    object ElimUniversalEquality extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
        case orig @ ComputationF(f: any.EQ, Vec2(x, y), _) =>
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
                  val recursed =
                    optim(retrieve, record)(ComputationF(any.EQ, Vec(a, b), Tag.ofBoolean))
                  record(recursed)
                })
                ComputationF(bool.And, pushedDown, Tag.ofBoolean)
              }
            case (ProductF(xs, xt), ProductF(ys, yt)) =>
//              require(xt == yt)
              require(xs.size == ys.size)
              val pushedDown = xs.indices.map(i => {
                val a = xs(i)
                val b = ys(i)
                val recursed =
                  optim(retrieve, record)(ComputationF(any.EQ, Vec(a, b), Tag.ofBoolean))
                record(recursed)
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

        case x => x
      }
    }

    object ElimReversible extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
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

    object SimplifyIfThenElse extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
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

    object ElimTautologies extends Optimizer {
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
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
        // note: those two cases can be extended to vectors of arbitrary size
        case original @ ComputationF(bool.Or, Vec2(a, b), _) =>
          if(b == record(ComputationF(bool.Not, Vec(a), Tag.ofBoolean)))
            bool.TrueF
          else if(a == record(ComputationF(bool.Not, Vec(b), Tag.ofBoolean)))
            bool.TrueF
          else
            original
        case original @ ComputationF(bool.And, Vec2(a, b), _) =>
          if(b == record(ComputationF(bool.Not, Vec(a), Tag.ofBoolean)))
            bool.FalseF
          else if(a == record(ComputationF(bool.Not, Vec(b), Tag.ofBoolean)))
            bool.FalseF
          else
            original

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
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
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
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
        case x @ ComputationF(f: Monoid[_], args, _) if args.isEmpty => f.liftedIdentity
        case x @ ComputationF(_: Monoid[_], Vec1(arg), _)            => retrieve(arg)
        case x                                                       => x
      }
    }

    object ElimDuplicationsIdempotentMonoids extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
        case ComputationF(f: IdempotentMonoid[_], args, tpe)
            if f.isInstanceOf[CommutativeMonoid[_]] =>
          ComputationF(f, args.distinct, tpe)
        case x => x
      }
    }

    object FlattenMonoids extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
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

    object ConstantFolding extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = o => {
        val TRUE: IDTop = record(bool.TrueF)
        val FALSE: IDTop = record(bool.FalseF)

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
    }

    object OrderArgs extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
        case x @ ComputationF(f: CommutativeMonoid[_], args, tpe) =>
          if(args.isSorted)
            x
          else
            ComputationF(f, args.sorted, tpe)
        case x => x
      }
    }

    object ExtractField extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
        case x @ ComputationF(fa: FieldAccess[_, _], Vec1(p), _) =>
          retrieve(p) match {
            case ProductF(members, _) =>
              retrieve(members(fa.fieldPosition))
            case _ => x
          }
        case ComputationF(fa: FieldAccess[_, _], _, _) => unexpected
        case x                                         => x
      }
    }

    object DistributeImplication extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] = {
        case original @ ComputationF(bool.Or, Vec2(a, b), _) =>
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

    private val optimizers = List(
      ExtractField,
      ElimUniversalEquality,
      ElimReversible,
      ElimIdentity,
      ElimEmptyAndSingletonMonoid,
      FlattenMonoids,
      ElimDuplicationsIdempotentMonoids,
      ElimTautologies,
      ConstantFolding,
      SimplifyIfThenElse,
      DistributeImplication,
      OrderArgs,
    )
    val optimizer: Optimizer =
      (optimizers ++ optimizers ++ optimizers).foldLeft[Optimizer](NoOpOptimizer) {
        case (acc, next) => acc.andThen(next)
      }
  }

  final class Utils(val directRec: Total[IDTop] => IDTop, retrieve: IDTop => Total[IDTop]) {
    def rec(expr: Total[IDTop]): IDTop =
      directRec(Optimizations.optimizer.optim(retrieve, directRec)(expr))
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
          def isReallyValid(ir: IR[IDTop]) = utils.implies(ir.present, ir.valid)
          val validity =
            utils.and(utils.or(utils.not(present.value), isReallyValid(value)),
                      isReallyValid(present))
          val presence =
            utils.or(utils.and(value.present, present.present, present.value), utils.not(validity))
          IR(
            value = value.value,
            present = presence,
            valid = validity
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
          IR(
            value = value.value,
            present = value.present,
            valid = utils.and(
              utils.implies(value.present, value.valid),
              utils.implies(condition.present, utils.and(condition.value, condition.valid)))
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
//  type TMP[X] = (ConstrainedF[X], ConstrainedF[X])
//  def encodePresence[X](
//      _lt: IlazyForest[X, NoApplyF, cats.Id, _]): IlazyForest[X, TMP, cats.Id, _] = {
//    val lt = _lt.fixID
//    val XX = lt.mapInternal[TMP[lt.ID]](???)
//    XX
//  }

  def encodePresence[X <: Int](root: X,
                               coalgebra: FCoalgebra[NoApplyF, X],
                               optimize: Boolean = true): LazyTree[X, ConstrainedF, Prez, _] = {
    val lt = IlazyForest.build(coalgebra)(compilerPresence).fixID

    LazyTree(lt)(root)
  }

  final class UtilsPrez(val directRec: ConstrainedF[IDTop] => IDTop,
                        retrieve: IDTop => ConstrainedF[IDTop]) {
    def rec(expr: ConstrainedF[IDTop]): IDTop = directRec(expr)
    def ret(i: IDTop): ConstrainedF[IDTop] = retrieve(i)

    val TRUE: IDTop = rec(CstF(Value(true), Tag.ofBoolean))
    val FALSE: IDTop = rec(CstF(Value(false), Tag.ofBoolean))

    def and(conjuncts: IDTop*): IDTop = and(Vec.unsafe(conjuncts.toArray))
    def and(conjuncts: Vec[IDTop]): IDTop = {
      val noTrue = conjuncts.filter(_ != TRUE)
      if(noTrue.isEmpty) TRUE
      else if(noTrue.size == 1) noTrue(0)
      else rec(ComputationF(bool.And, noTrue, Tag.ofBoolean))
    }
    def or(disjuncts: IDTop*): IDTop =
      rec(ComputationF(bool.Or, Vec.unsafe(disjuncts.toArray), Tag.ofBoolean))
    def not(e: IDTop): IDTop =
      rec(ComputationF(bool.Not, Seq(e), Tag.ofBoolean))

    def implies(cond: IDTop, eff: IDTop): IDTop =
      or(not(cond), eff)

  }

  final case class Prez[@sp(Int) F](value: F, present: F)
  object Prez {
    implicit object functorInstance extends Functor[Prez] {
      override def map[A, B](fa: Prez[A])(f: A => B): Prez[B] = Prez(f(fa.value), f(fa.present))
    }
  }

  def compilerPresence(context: LazyForestGenerator.Context[ConstrainedF, IDTop])
    : StaticF[Prez[IDTop]] => Prez[IDTop] =
    x => {
      val utils = new UtilsPrez(context.record, context.retrieve)
      val ir: Prez[IDTop] = x match {
        case x: InputF[_] => Prez(utils.rec(x), utils.TRUE)
        case x: CstF[_]   => Prez(utils.rec(x), utils.TRUE)
        case ComputationF(f, args, t) =>
          Prez(
            value = utils.rec(ComputationF(f, args.map(a => a.value), t)),
            present = utils.and(args.map(_.present))
          )
        case ProductF(members, t) =>
          Prez(
            value = utils.rec(ProductF(members.map(a => a.value), t)),
            present = utils.and(members.map(_.present))
          )
        case SequenceF(members, t) =>
          Prez(
            value = utils.rec(SequenceF(members.map(a => a.value), t)),
            present = utils.and(members.map(_.present))
          )
        case ITEF(cond, onTrue, onFalse, t) =>
          Prez(
            value = utils.rec(ITEF(cond.value, onTrue.value, onFalse.value, t)),
            present = utils.and(cond.present,
                                utils.implies(cond.value, onTrue.present),
                                utils.implies(utils.not(cond.value), onFalse.present))
          )
        case OptionalF(value, present, _) =>
          Prez(
            value = value.value,
            present = utils.and(value.present, present.present, present.value)
          )
        case PresentF(opt) =>
          Prez(
            value = opt.present,
            present = utils.TRUE
          )
        case ValidF(part) =>
          Prez(
            value = utils.rec(ValidF(part.value)),
            present = part.present
          )
        case Partial(value, condition, tpe) =>
          Prez(
            value = utils.rec(Partial(value.value, condition.value, tpe)),
            present = utils.and(value.present, condition.present)
          )
        case p @ LambdaParamF(id, typ) =>
          val placeHolder = utils.rec(LambdaParamF(id, typ))
          Prez(
            value = utils.rec(ComputationF(TotalValue.Value, Vec(placeHolder), typ)),
            present = utils.rec(ComputationF(TotalValue.Present, Vec(placeHolder), Tag.ofBoolean))
          )
        case LambdaF(in, tree, id, tpe) =>
          val presentValidTag = LambdaTag.LambdaTagImpl(tpe, Tag.ofBoolean)
          Prez(
            value = utils.rec(LambdaF(in.value, tree.value, id, tpe)),
            present = utils.rec(LambdaF(in.value, tree.present, id, presentValidTag))
          )
        case NoopF(expr, _) =>
          Prez(
            value = expr.value,
            present = expr.present
          )
      }
      ir
    }

}
