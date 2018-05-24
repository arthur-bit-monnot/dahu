package dahu.model.problem

import cats.Functor
import cats.implicits._
import dahu.model.functions.{Box, Reversible, Unbox}
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

//  def satisfactionSubAST(ast: AST[_]): LazyTree[ast.ID, Total, cats.Id, _] = {
//    encode(ast.root, ast.tree.asFunction).mapExternal[cats.Id](_.valid)
//  }

  object Optimizations {
    trait Optimizer {
      self =>
      def optim(retrieve: IDTop => Total[IDTop],
                record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop]

      def andThen(next: Optimizer): Optimizer = new Optimizer {
        override def optim(retrieve: IDTop => Total[IDTop],
                           record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] =
          self.optim(retrieve, record) andThen next.optim(retrieve, record)

        override def toString: String = s"self >> next"
      }
    }

    object NoOpOptimizer extends Optimizer {
      override def optim(retrieve: IDTop => Total[IDTop],
                         record: Total[IDTop] => IDTop): Total[IDTop] => Total[IDTop] =
        identity[Total[IDTop]]
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
            case _ => unexpected
          }
        case ComputationF(fa: FieldAccess[_, _], _, _) => unexpected
        case x                                         => x
      }
    }

    private val optimizers = List(
      ExtractField,
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

  final class Utils(val directRec: Total[IDTop] => IDTop, retrieve: IDTop => Total[IDTop]) {
    def rec(expr: Total[IDTop]): IDTop =
      directRec(Optimizations.optimizer.optim(retrieve, directRec)(expr))

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

  case class IR[@specialized(Int) A](value: A, present: A, valid: A)
  object IR {
    implicit val functorInstance: Functor[IR] = new Functor[IR] {
      override def map[A, B](fa: IR[A])(f: A => B): IR[B] =
        IR(f(fa.value), f(fa.present), f(fa.valid))
    }
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
          IR(
            value = value.value,
            present = utils.and(value.present, present.present, present.value),
            valid = utils.and(present.valid, value.valid)
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
              value.valid,
              utils.implies(condition.present, utils.and(condition.value, condition.valid)))
          )
      }
      ir
    }

  // TODO: the ExprF is to wide as this method does not handle lambdas
  def encode[X <: Int](root: X,
                       coalgebra: FCoalgebra[StaticF, X],
                       optimize: Boolean = true): LazyTree[X, Total, IR, _] = {
    val lt = IlazyForest.build(coalgebra, compiler).fixID

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

}
