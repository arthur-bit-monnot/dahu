package dahu.model.transformations

import dahu.model.math.{bool, CommutativeMonoid}
import dahu.model.types.Value
import dahu.utils._
import cats.implicits._
import cats.kernel.Order
import dahu.model.functions._
import dahu.model.ir._
import dahu.model.math._
import dahu.model.products.{Constructor, Field, FieldAccess, GetField}
import dahu.model.types._
import spire.math.Interval
import spire.syntax.cfor._

/** A pass implements a transformation from a an expression F[I] to an another expression of the same form.
  *
  * A pass is covariant on the in the expression it can handle: a Pass[StaticF] is also a Pass[ExprF].
  * Hence specialized passed can be used to transform more general trees.
  *
  * TODO: we might be able to remove the Lower bound on the supported expression.
  *
  * @param name Human readable name of the transformation.
  * @tparam E Lower bound on the supported expression type.
  */
abstract class Pass[+E[X] >: Total[X] <: ExprF[X]](val name: String) {
  implicit def order[I <: Int]: Order[I] = Order[Int].asInstanceOf[Order[I]]

  def optim[I <: Int, F[X] >: E[X] <: ExprF[X]](
      implicit ctx: OptimizationContext[I, F]): F[I] => F[I]
}

final class ComposedPass[LB[X] >: Total[X] <: ExprF[X]](passes: Seq[Pass[LB]])
    extends Pass[LB](passes.map(_.name).mkString(" -> ")) {
  override def optim[I <: Int, F[X] >: LB[X] <: ExprF[X]](
      implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
    val transformations: Seq[F[I] => F[I]] = passes.map(p => p.optim(ctx))
    val full = transformations.foldLeft(identity[F[I]](_))((comb, next) => comb.andThen(next))
    full
  }
}

object Pass {

  private def isBox(f: FunAny): Boolean = f match {
    case fr: Reversible[_, _] if fr.name == "box" => true
    case _                                        => false
  }
  private def isUnbox(f: FunAny): Boolean = f match {
    case fr: Reversible[_, _] if fr.name == "unbox" => true
    case _                                          => false
  }

  final val extractTypes: Pass[Total] = new Pass[Total]("extract-types") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {

      case orig @ ComputationF(any.TypeOf, Vec(a), _) =>
        val arg = ctx.retrieve(a)
        if(arg.typ == Tag.unsafe.ofAny) {
          if(!arg.isInstanceOf[LambdaParamF[_]]) {
            val XX = arg.asInstanceOf[ExprF[I]].smap[ExprF[I]](ctx.retrieve)
            println("stop" + XX)
          }
          orig
        } else {
          CstF(Value(arg.typ), Tag.ofType)
        }
      case x => x
    }
  }

  final val distBoxOverITE: Pass[Total] = new Pass[Total]("distribute-box-over-ite") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case orig @ ComputationF(f, Vec(a), t) if isBox(f) || isUnbox(f) =>
        ctx.retrieve(a) match {
          case ITEF(cond, onTrue, onFalse, typ) =>
            val trueCase = ctx.record(ComputationF(f, Vec(onTrue), t))
            val falseCase = ctx.record(ComputationF(f, Vec(onFalse), t))
            ITEF(cond, trueCase, falseCase, t)
          case _ => orig
        }
      case x => x
    }
  }
  import dahu.utils.errors._

  final val foldConstants: Pass[Total] = new Pass[Total]("fold-constants") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case ComputationF(bool.And, args, _) if args.contains(ctx.FALSE) => ctx.retrieve(ctx.FALSE)
      case ComputationF(bool.Or, args, _) if args.contains(ctx.TRUE)   => ctx.retrieve(ctx.TRUE)
      // commutative monoid, evaluate the combination of all constants args

      // any function, evaluate if all args are constant
      case ComputationF(f, args, t)
          if args.forall(ctx.retrieve(_).isInstanceOf[CstF[_]])
            && !t.isInstanceOf[SequenceTagAny] => // TODO: SequenceF should be our Vec type
        val params = args.map(i =>
          ctx.retrieve(i) match {
            case CstF(value, _) => value
            case _              => unexpected
        })
        CstF(Value(f.compute(params)), t)

      case ComputationF(f: CommutativeMonoid[_], args, tpe)
          if args.count(ctx.retrieve(_).isInstanceOf[CstF[_]]) >= 2 =>
        val buff = debox.Buffer[I]()
        var acc: Value = Value(f.identity)
        cfor(0)(_ < args.length, _ + 1) { i =>
          ctx.retrieve(args(i)) match {
            case CstF(value, _) => acc = f.combineUnsafe(acc, value)
            case _              => buff += args(i)
          }
        }
        buff += ctx.record(CstF(acc, f.outType))
        ComputationF(f, Vec.unsafe(buff.toArray()), tpe)

      case x @ ITEF(cond, onTrue, onFalse, _) =>
        if(cond == ctx.TRUE) ctx.retrieve(onTrue)
        else if(cond == ctx.FALSE) ctx.retrieve(onFalse)
        else x

      case x => x
    }
  }

//  final class ElimUniversalEquality(ctx: OptimizationContext) extends Optimizer2(ctx) {
//    override def optimImpl(fi: Total[SomeID]): Total[SomeID] =
  //
  final val elimUniEq: Pass[Total] = new Pass[Total]("elim-universal-equality") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case orig @ ComputationF(f: any.EQ, Vec(x, y), _) if x == y => bool.TrueF
      case orig @ ComputationF(f: any.EQ, Vec(x, y), _) =>
        val fx = ctx.retrieve(x)
        val fy = ctx.retrieve(y)
        (fx, fy) match {
          case (SequenceF(xs, xt), SequenceF(ys, yt)) =>
//              require(xt == yt) // TODO: we should be able to test type equality for derived types
            if(xs.size != ys.size)
              bool.FalseF
            else {
              val pushedDown = xs.indices.map(i => {
                val a = xs(i)
                val b = ys(i)
                ctx.record(ComputationF(any.EQ, Vec(a, b), Tag.ofBoolean))
              })
              ComputationF(bool.And, pushedDown, Tag.ofBoolean)
            }
          case (ProductF(xs, xt), ProductF(ys, yt)) =>
//              require(xt == yt)
            require(xs.size == ys.size)
            val pushedDown = xs.indices.map(i => {
              val a = xs(i)
              val b = ys(i)
              ctx.record(ComputationF(any.EQ, Vec(a, b), Tag.ofBoolean))
            })
            ComputationF(bool.And, pushedDown, Tag.ofBoolean)
          case (CstF(a, _), CstF(b, _)) =>
            if(a == b) bool.TrueF else bool.FalseF

          case _ if fx.typ == Tag.ofInt && fy.typ == Tag.ofInt =>
            ComputationF(int.EQ, Vec(x, y), Tag.ofBoolean)
          case _ =>
            (fx.typ, fy.typ) match {
              case (t1: TagIsoInt[_], t2: TagIsoInt[_]) =>
                val ix = ctx.record(ComputationF(t1.unbox, Vec(x), t1.unbox.outType))
                val iy = ctx.record(ComputationF(t2.unbox, Vec(y), t1.unbox.outType))
                ComputationF(int.EQ, Vec(ix, iy), int.EQ.outType)
              case (Tag.ofDouble, Tag.ofDouble) =>
                ComputationF(double.EQ, x, y)
              case _ =>
//                  dahu.utils.debug.warning(s"Universal equality not specialized: $fx == $fy")
                orig
            }

        }

      case x =>
        x
    }
  }

  final val elimReversible: Pass[Total] = new Pass[Total]("elim-reversible") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case orig @ ComputationF(f: Reversible[_, _], Vec(arg), _) =>
        ctx.retrieve(arg) match {
          case ComputationF(f2: Reversible[_, _], Vec(arg2), _) if f2 == f.reverse =>
            ctx.retrieve(arg2)
          case ComputationF(f2: Box[_], _, _) => ???
          case _                              => orig
        }
      case x => x
    }
  }

  final val simplifyITE: Pass[Total] = new Pass[Total]("simplify-ite") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case ITEF(_, onTrue, onFalse, _) if onTrue == onFalse => ctx.retrieve(onTrue)
      case orig @ ITEF(cond, onTrue, onFalse, t) if t == Tag.ofBoolean =>
        if(ctx.retrieve(onFalse) == bool.FalseF)
          ComputationF(bool.And, Vec(cond, onTrue), Tag.ofBoolean)
        else if(ctx.retrieve(onFalse) == bool.TrueF) {
          val notCond = ctx.record(ComputationF(bool.Not, Vec(cond), Tag.ofBoolean))
          ComputationF(bool.Or, Vec(notCond, onTrue), Tag.ofBoolean)
        } else
          orig
      case x => x
    }
  }

  import dahu.model.problem.syntax._

  final val elimTauto: Pass[Total] = new Pass[Total]("elim-tauto") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = fi => {
      def tagDom(t: TagAny): Interval[Int] = t match {
        case t: TagIsoInt[_] => Interval(t.min, t.max)
        case _               => Interval.all
      }
      def dom(e: ExprF[I]): Interval[Int] = e match {
        case ComputationF(f: Unbox[_], _, _) =>
          tagDom(f.inType) intersect tagDom(f.outType)
        case CstF(v: Int, _) =>
          Interval.point(v)
        case x =>
          tagDom(x.typ)
      }
      import ctx._
      (fi: F[I]) match {
        // note: those two cases can be extended to vectors of arbitrary size
        case ComputationF(bool.Or, Vec(a, b), _) =>
          if(b == ctx.record(ComputationF(bool.Not, Vec(a), Tag.ofBoolean)))
            bool.TrueF
          else if(a == ctx.record(ComputationF(bool.Not, Vec(b), Tag.ofBoolean)))
            bool.TrueF
          else
            fi
        case ComputationF(bool.And, Vec(a, b), _) =>
          if(b == record(ComputationF(bool.Not, Vec(a), Tag.ofBoolean)))
            bool.FalseF
          else if(a == record(ComputationF(bool.Not, Vec(b), Tag.ofBoolean)))
            bool.FalseF
          else
            fi
//
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
  }

  final val elimIdent: Pass[Total] = new Pass[Total]("elim-identity") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case x @ ComputationF(f: Monoid[_], args, tpe) =>
        val identity = ctx.record(f.liftedIdentity)
        if(args.contains(identity))
          ComputationF(f, args.filter(_ != identity), tpe)
        else
          x
      case x => x

    }
  }

  final val elimEmptySingleMonoids: Pass[Total] = new Pass[Total]("elim-empty-single-monoid") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case x @ ComputationF(f: Monoid[_], args, _) if args.isEmpty => f.liftedIdentity
      case x @ ComputationF(_: Monoid[_], Vec(arg), _)             => ctx.retrieve(arg)
      case x                                                       => x
    }
  }

  final val elimDupIdempotent: Pass[Total] = new Pass[Total]("elim-dup-idempotent") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case ComputationF(f: IdempotentMonoid[_], args, tpe)
          if f.isInstanceOf[CommutativeMonoid[_]] =>
        ComputationF(f, args.distinct, tpe)
      case x => x
    }
  }

  final val flattenMonoids: Pass[Total] = new Pass[Total]("flatten-monoids") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case x @ ComputationF(f: Monoid[_], args, tpe) =>
        val buff = debox.Buffer[I]()
        cfor(0)(_ < args.length, _ + 1) { i =>
          ctx.retrieve(args(i)) match {
            case ComputationF(f2: Monoid[_], args2, _) if f2 == f =>
              args2.foreach(buff += _)
            case _ =>
              buff += args(i)
          }
        }
        if(buff.length != args.size)
          ComputationF[I](f, Vec.unsafe(buff.toArray()), tpe)
        else
          x
      case x => x
    }
  }

  final val orderArgs: Pass[Total] = new Pass[Total]("order-args") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case x @ ComputationF(f: CommutativeMonoid[_], args, tpe) =>
        if(args.isSorted)
          x
        else
          ComputationF(f, args.sorted, tpe)
      case x => x
    }
  }

  final val elimNoops: Pass[Total] = new Pass[Total]("elim-noops") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case NoopF(e, _) => ctx.retrieve(e)
      case x           => x
    }
  }

  final val extractField: Pass[Total] = new Pass[Total]("extract-field") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {

      case x @ ComputationF(ctor: Constructor, args, _) =>
        ProductF(args, ctor.outType)

      case x @ ComputationF(ctor: sequence.ListBuilder[_], args, _) =>
        SequenceF(args, ctor.outType)

      case x @ ComputationF(fa: FieldAccess[_, _], Vec(p), _) =>
        ctx.retrieve(p) match {
          case ProductF(members, _) =>
            ctx.retrieve(members(fa.fieldPosition))
          case unmatched =>
//              debug.warning(s"Field was not extracted: $fa of $unmatched")
            x
        }
      case ComputationF(fa: FieldAccess[_, _], _, _) => unexpected
      case x @ ComputationF(fa: GetField, Vec(p), _) =>
        ctx.retrieve(p) match {
          case ProductF(members, tpe) =>
            tpe.fields.toSeq.find(_.name == fa.fieldName) match {
              case Some(Field(_, _, position)) =>
                ctx.retrieve(members(position))
              case None =>
                x
            }

          case unmatched =>
            x
        }
      case x => x
    }
  }

  final val distNot: Pass[Total] = new Pass[Total]("distribute-not") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = fi => {
      def not(a: I): I = {
        ctx.retrieve(a) match {
          case Not(na) => na
          case _       => ctx.record(Not(a))
        }
      }

      fi match {
        case original @ Not(a) =>
          ctx.retrieve(a) match {
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
  }

  final val evalSequenceOps: Pass[Total] = new Pass[Total]("eval-sequence-ops") {
    def optim[I <: Int, F[X] >: Total[X] <: ExprF[X]](
        implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
      case fi @ ComputationF(sequence.Size, Vec(e), _) =>
        ctx.retrieve(e) match {
          case SequenceF(vec, _) => CstF(Value(vec.size), Tag.ofInt)
          case _                 => fi
        }

      case fi @ ComputationF(_: sequence.First[_], Vec(seq), _) =>
        ctx.retrieve(seq) match {
          case SequenceF(members, _) =>
            require(members.size > 0)
            ctx.retrieve(members.firstUnsafe)
          case ComputationF(_: sequence.Concat[_], args, _) if args.nonEmpty =>
            ctx.retrieve(args.firstUnsafe) match {
              case SequenceF(members, _) if members.nonEmpty =>
                ctx.retrieve(members.firstUnsafe)
              case _ => fi
            }
          case _ => fi

        }

      case fi @ ComputationF(_: sequence.Last[_], Vec(seq), _) =>
        ctx.retrieve(seq) match {
          case SequenceF(members, _) if members.nonEmpty =>
            ctx.retrieve(members.lastUnsafe)
          case ComputationF(_: sequence.Concat[_], args, _) if args.nonEmpty =>
            ctx.retrieve(args.lastUnsafe) match {
              case SequenceF(members, _) if members.nonEmpty =>
                ctx.retrieve(members.lastUnsafe)
              case _ =>
                fi
            }
          case _ =>
            fi
        }

      case fi => fi
    }
  }

  final val expandFirstOrderFunction: Pass[StaticF] =
    new Pass[StaticF]("expand-1st-order-functions") {
      def optim[I <: Int, F[X] >: StaticF[X] <: ExprF[X]](
          implicit ctx: OptimizationContext[I, F]): F[I] => F[I] = {
        case fi @ ComputationF(map: sequence.Map[_, _], Vec(fun, seq), _) =>
          ctx.retrieve(seq) match {
            case SequenceF(members: Vec[I], _) =>
              val x: Vec[ApplyF[I]] = members.map(i => ApplyF[I](fun, i, null))
              val is: Vec[I] = x.map(ctx.record)
              SequenceF(is, null)

            case CstF(values: Vec[Value], _) => // TODO: SequenceF should match a CstF
              val members = values.map(value => ctx.record(CstF(value, Tag.unsafe.ofAny)))
              val x: Vec[ApplyF[I]] = members.map(i => ApplyF[I](fun, i, null))
              val is: Vec[I] = x.map(ctx.record)
              SequenceF(is, null)

            case ComputationF(_: sequence.Concat[_], lists, _) =>
              val mappedLists = lists.map(l => ctx.record(ComputationF(map, fun, l)))
              ComputationF(sequence.Concat(Tag.unsafe.ofAny), mappedLists)
            case _ => fi
          }
        case fi @ ComputationF(fold @ sequence.Fold(monoid), Vec(seq), _) =>
          ctx.retrieve(seq) match {
            case SequenceF(members: Vec[I], _) =>
              ComputationF(monoid, members, monoid.tpe)

            case ComputationF(cn: sequence.Concat[_], lists, _) =>
              val foldedSubs = lists.map(l => ctx.record(ComputationF(fold, l)))
              ComputationF(monoid, foldedSubs)

            case _ => fi
          }

        case orig @ ApplyF(lbd, param, _) =>
          ctx.retrieve(lbd) match {
            case CstF(f: FunAny, _) if f.arity.contains(1) =>
              ComputationF(f, param)
            case _ => orig
          }

        case fi @ ComputationF(_: sequence.AllConsecutive[_], Vec(vec, f), _) =>
          ctx.retrieve(vec) match {
            case SequenceF(vec, _) =>
              val conjuncts: Seq[I] = for(i <- 0 until vec.size - 1) yield {
                val a = vec(i)
                val b = vec(i + 1)
                // f(a) (b)
                val fa = ctx.record(ApplyF(f, a, null)) //TODO
                val fab = ctx.record(ApplyF(fa, b, Tag.ofBoolean))
                fab
              }

              ComputationF(bool.And, conjuncts: _*)

            case _ =>
              fi
          }
        case fi @ ComputationF(mc2: sequence.MapConsecutive2[_, _], Vec(f, vec), _) =>
          ctx.retrieve(vec) match {
            case SequenceF(vec, _) =>
              val conjuncts: Seq[I] = for(i <- 0 until vec.size - 1) yield {
                val a = vec(i)
                val b = vec(i + 1)
                // f(a) (b)
                val fa = ctx.record(ApplyF(f, a, Tag.unsafe.ofAny)) //TODO
                val fab = ctx.record(ApplyF(fa, b, Tag.unsafe.ofAny))
                fab
              }

              SequenceF(conjuncts.toVec, mc2.outType)

            case _ =>
              fi
          }
        case fi @ ComputationF(mc2: sequence.MapConsecutive3[_, _], Vec(f, vec), _) =>
          ctx.retrieve(vec) match {
            case SequenceF(vec, _) =>
              val conjuncts: Seq[I] = for(i <- 0 until vec.size - 2) yield {
                val a = vec(i)
                val b = vec(i + 1)
                val c = vec(i + 2)
                // f(a) (b)
                val fa = ctx.record(ApplyF(f, a, Tag.unsafe.ofAny)) //TODO
                val fab = ctx.record(ApplyF(fa, b, Tag.unsafe.ofAny))
                val fabc = ctx.record(ApplyF(fab, c, Tag.unsafe.ofAny))
                fabc
              }

              SequenceF(conjuncts.toVec, mc2.outType)

            case _ =>
              fi
          }

        case fi => fi

      }
    }

  val allTotalPasses: Seq[Pass[Total]] =
    Seq(
      extractTypes,
      distBoxOverITE,
      foldConstants,
      elimUniEq,
      elimReversible,
      simplifyITE,
      elimTauto,
      elimIdent,
      elimEmptySingleMonoids,
      elimDupIdempotent,
      flattenMonoids,
      orderArgs,
      elimNoops,
      extractField,
      distNot,
      evalSequenceOps
    )

  val pureStaticPasses: Seq[Pass[StaticF]] = Seq(expandFirstOrderFunction)
  val allStaticPasses: Seq[Pass[StaticF]] = totalPasses ++ pureStaticPasses

  final object Implications {
    // This is some transformation that deal with implications but not needed anymore given the current model.
    //  final class DistributeImplication(ctx: OptimizationContext) extends Optimizer2(ctx) {
    //    override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
    //      case original @ ComputationF(bool.Or, Vec(a, b), _) =>
    //        def tryWith(a: SomeID, b: SomeID): Option[Total[SomeID]] =
    //          (retrieve(a), retrieve(b)) match {
    //            case (fa @ ComputationF(bool.Not, _, _), ComputationF(bool.And, conjs, _)) =>
    //              val disjuncts =
    //                conjs.map(c => record(ComputationF(bool.Or, Vec(a, c), Tag.ofBoolean)))
    //              Some(ComputationF(bool.And, disjuncts, Tag.ofBoolean))
    //            case _ => None
    //          }
    //
    //        tryWith(a, b).orElse(tryWith(b, a)).getOrElse(original)
    //      case x => x
    //    }
    //  }
    //  final class GroupImplications(ctx: OptimizationContext) extends Optimizer2(ctx) {
    //    override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
    //      case original @ And(as @ _*) =>
    //        val implications = mutable.ArrayBuffer[(SomeID, SomeID)]()
    //        val others = mutable.ArrayBuffer[SomeID]()
    //        for(a <- as) {
    //          retrieve(a) match {
    //            case Or(x, y) =>
    //              (retrieve(x), retrieve(y)) match {
    //                case (Not(lhs), _) => implications += ((lhs, y))
    //                case (_, Not(rhs)) => implications += ((rhs, x))
    //                case _             => others += a
    //              }
    //            case _ => others += a
    //          }
    //
    //        }
    //        val gp = implications.groupBy(_._1).toSeq.sortBy(t => retrieve(t._1).toString)
    //        val implictationConjuncts = gp.map {
    //          case (lhs, rhss) =>
    //            record(Or(lhs, record(And(rhss.map(_._2)))))
    //        }
    //        And(others.sortBy(retrieve(_).toString) ++ implictationConjuncts)
    //      case x => x
    //    }
    //  }
    //
    //  final class SimplifyImplications(ctx: OptimizationContext) extends Optimizer2(ctx) {
    //    import ctx.{FALSE, TRUE}
    //    class ImplicationAccumulator(record: Total[SomeID] => SomeID) {
    //      private val conds = mutable.ArrayBuffer[Vec[SomeID]]()
    //      private val effs = mutable.ArrayBuffer[debox.Set[SomeID]]()
    //      def isForced(i: SomeID): Boolean = {
    //        if(conds.isEmpty || conds(0).size > 0)
    //          false
    //        else
    //          effs(0)(i)
    //      }
    //      def add(condition: Vec[SomeID], effects: Vec[SomeID]): Unit = {
    //        addToSame(condition, effects.filter(i => !isForced(i)))
    //      }
    //      def addToSame(condition: Vec[SomeID], effects: Vec[SomeID]): Unit = {
    //        val loc = conds.indexOf(condition)
    //
    //        if(loc == -1) {
    //          conds += condition
    //          effs += debox.Set.fromArray(effects.toArray)
    //        } else {
    //          effs(loc).addAll(effects.toArray)
    //        }
    //      }
    //      def makeAnd(effs: Iterable[SomeID]): SomeID = {
    //        if(effs.isEmpty)
    //          TRUE
    //        else if(effs.size == 1)
    //          effs.head
    //        else
    //          record(And(effs))
    //      }
    //      def makeNot(conds: Vec[SomeID]): SomeID = {
    //        if(conds.size == 0)
    //          FALSE
    //        else if(conds.size == 1)
    //          record(Not(conds(0)))
    //        else
    //          record(Not(makeAnd(conds.toIterable)))
    //      }
    //      def makeOr(a: SomeID, b: SomeID): SomeID =
    //        if(a == TRUE || b == TRUE)
    //          TRUE
    //        else if(a == FALSE)
    //          b
    //        else if(b == FALSE)
    //          a
    //        else
    //          record(Or(a, b))
    //      def makeimplication(condition: Vec[SomeID], effs: debox.Set[SomeID]): SomeID = {
    //        makeOr(makeNot(condition), makeAnd(effs.toIterable()))
    //      }
    //      def compile(): Total[SomeID] = {
    //        val l = conds.length
    //        val conjuncts = new Array[SomeID](l)
    //        spire.syntax.cfor.cfor(0)(_ < conds.length, _ + 1) { i =>
    //          conjuncts(i) = makeimplication(conds(i), effs(i))
    //        }
    //        And(conjuncts.toIterable)
    //      }
    //    }
    //
    //    override def optimImpl(fi: Total[SomeID]): Total[SomeID] = fi match {
    //      case original @ And(args @ _*) =>
    //        def notConj(i: SomeID): Option[Vec[SomeID]] =
    //          retrieve(i) match {
    //            case Not(a) =>
    //              retrieve(a) match {
    //                case And(notConj @ _*) => Some(notConj.toVec)
    //                case _                 => Some(Vec(a))
    //              }
    //            case _ => None
    //          }
    //        def anyConj(i: SomeID): Vec[SomeID] =
    //          retrieve(i) match {
    //            case ComputationF(bool.And, args, _) => args
    //            case _                               => Vec(i)
    //          }
    //        def asImplication(i: SomeID): (Vec[SomeID], Vec[SomeID]) = {
    //          retrieve(i) match {
    //            case Or(a, b) =>
    //              notConj(a) match {
    //                case Some(conditions) =>
    //                  (conditions, anyConj(b))
    //                case None =>
    //                  notConj(b) match {
    //                    case Some(condtions) => (condtions, anyConj(a))
    //                    case None            => (Vec.empty, Vec(i))
    //                  }
    //              }
    //            case _ => (Vec.empty, Vec(i))
    //          }
    //        }
    //
    //        val implications = args.map(asImplication).sortBy(_._1.size)
    //        val acc = new ImplicationAccumulator(record)
    //        implications.foreach(t => acc.add(t._1, t._2))
    //        acc.compile()
    //      case x => x
    //    }
    //  }
  }
}
