package dahu.planning.planner

import dahu.model.functions.{->:, Fun2, FunN, Reversible}
import dahu.model.input.{Expr => Tentative, _}
import dahu.model.input.dsl._
import dahu.model.math.{bool, int}
import dahu.model.products.FieldAccess
import dahu.model.types.{BoxedInt, ProductTag, Tag, TagIsoInt}
import dahu.planning.model.common.operators.BinaryOperator
import dahu.planning.model.common.{Cst => _, _}
import dahu.planning.model.core._
import dahu.planning.model.{common, core}
import dahu.utils.errors._

import scala.collection.mutable

case class Fluent(template: FunctionTemplate, args: Seq[Tentative[Literal]])
case class Constant(template: ConstantTemplate, args: Seq[Tentative[Literal]])

case class ConditionToken(itv: Interval[Tentative[Int]], fluent: Fluent, value: Tentative[Literal])

case class EffectToken(changeItv: Interval[Tentative[Int]],
                       persistenceEnd: Tentative[Int],
                       fluent: Fluent,
                       value: Tentative[Literal])

sealed trait Literal {
  def asConstant(tag: TagIsoInt[Literal]): Tentative[Literal] = Cst(this)(tag)
}
case class IntLit(value: Int) extends Literal {
  override def toString: String = value.toString
}
case class ObjLit(value: Instance) extends Literal {
  override def toString: String = value.toString
}

case class ProblemContext(intTag: BoxedInt[Literal],
                          topTag: TagIsoInt[Literal],
                          specializedTags: Type => TagIsoInt[Literal])(implicit predef: Predef) {

  def intBox(tpe: TagIsoInt[Literal], i: Tentative[Int]): Tentative[Literal] =
    Computation(tpe.box, i)
  def intUnbox(i: Tentative[Literal]): Tentative[Int] = {
    i.typ match {
      case t: TagIsoInt[Literal] => Computation(t.unbox, i)
      case _                     => unexpected
    }
  }
  private val booleanTag = specializedTags(predef.Boolean)
  val boolBoxing = new Reversible[Boolean, Literal]()(Tag[Boolean], booleanTag) { self =>
    override def reverse: Reversible[Literal, Boolean] =
      new Reversible[Literal, Boolean]()(booleanTag, Tag[Boolean]) {
        override def reverse: Reversible[Boolean, Literal] = self
        override def of(in: Literal): Boolean = in match {
          case ObjLit(predef.True)  => true
          case ObjLit(predef.False) => false
          case _                    => unexpected
        }
        override def name: String = "unbox"
      }
    override def of(in: Boolean): Literal = if(in) ObjLit(predef.True) else ObjLit(predef.False)
    override def name: String = "box"
  }
  def boolUnbox(i: Tentative[Literal]): Tentative[Boolean] = Computation(boolBoxing.reverse, i)
  def boolBox(i: Tentative[Boolean]): Tentative[Literal] = Computation(boolBoxing, i)

  def encode(v: common.Term)(implicit argRewrite: Arg => Tentative[Literal]): Tentative[Literal] =
    v match {
      case IntLiteral(i) => IntLit(i).asConstant(intTag)
      case lv @ LocalVar(id, tpe) if tpe.isSubtypeOf(predef.Time) =>
        assert(tpe == predef.Time)
        val e: Tentative[Int] = lv match {
          case predef.Start => temporalOrigin
          case predef.End   => temporalHorizon
          case _ =>
            Input[Int](Ident(lv)).subjectTo(tp => temporalOrigin <= tp && tp <= temporalHorizon)
        }
        intBox(intTag, e)
      case lv @ LocalVar(_, tpe) if tpe.isSubtypeOf(Type.Integers) =>
        assert(tpe == Type.Integers)
        Input[Literal](Ident(lv))(intTag)
      case lv @ LocalVar(_, tpe) =>
        assert(!tpe.isSubtypeOf(Type.Reals))
        Input[Literal](Ident(lv))(specializedTags(tpe))
      case i @ Instance(_, tpe) => ObjLit(i).asConstant(specializedTags(tpe))
      case a: Arg               => argRewrite(a)
    }

  def encode(e: common.Expr)(implicit argRewrite: Arg => Tentative[Literal]): Tentative[Literal] =
    e match {
      case term: common.Term => encode(term)
      case Op2(op, left, right) =>
        applyOperator(op, encode(left), encode(right))
    }
  def encodeAsInt(e: common.Expr)(
      implicit argRewrite: Arg => Tentative[Literal]): Tentative[Int] = {
    assert(e.typ.isSubtypeOf(Type.Integers))
    intUnbox(encode(e))
  }
  def encodeAsInts(e: common.Interval[common.Expr])(
      implicit argRewrite: Arg => Tentative[Literal]): Interval[Tentative[Int]] = {
    e.map(encodeAsInt)
  }
  def applyOperator(op: BinaryOperator,
                    lhs: Tentative[Literal],
                    rhs: Tentative[Literal]): Tentative[Literal] = op match {
    case operators.Eq  => lhs === rhs
    case operators.LEQ => lhs <= rhs
    case operators.LT  => lhs < rhs
    case operators.GEQ => rhs <= lhs
    case operators.GT  => rhs < lhs
    case operators.Add => lhs + rhs
    case operators.Sub => lhs - rhs
    case operators.And => lhs && rhs
    case operators.Or  => lhs || rhs
  }
  implicit class LitOps(private val lhs: Tentative[Literal]) {
    def ===(rhs: Tentative[Literal]): Tentative[Literal] = liftIIB(int.EQ)(lhs, rhs)
    def <=(rhs: Tentative[Literal]): Tentative[Literal] = liftIIB(int.LEQ)(lhs, rhs)
    def <(rhs: Tentative[Literal]): Tentative[Literal] = liftIIB(int.LEQ)(lhs, rhs + 1)
    def +(rhs: Tentative[Literal]): Tentative[Literal] = lift(int.Add)(lhs, rhs)
    def +(rhs: Int): Tentative[Literal] = lift(int.Add)(lhs, IntLit(rhs).asConstant(intTag))
    def -(rhs: Tentative[Literal]): Tentative[Literal] = lhs + (-rhs)
    def unary_-(): Tentative[Literal] = intBox(intTag, Computation(int.Negate, intUnbox(lhs)))
    def &&(rhs: Tentative[Literal]): Tentative[Literal] = liftNBB(bool.And)(lhs, rhs)
    def ||(rhs: Tentative[Literal]): Tentative[Literal] = liftNBB(bool.Or)(lhs, rhs)
  }

  def liftIII(
      f: Fun2[Int, Int, Int]): (Tentative[Literal], Tentative[Literal]) => Tentative[Literal] = {
    case (a1, a2) => intBox(intTag, Computation2(f, intUnbox(a1), intUnbox(a2)))
  }
  def liftIIB(f: Fun2[Int, Int, Boolean])
    : (Tentative[Literal], Tentative[Literal]) => Tentative[Literal] = {
    case (a1, a2) => boolBox(Computation2(f, intUnbox(a1), intUnbox(a2)))
  }
  def lift(f: FunN[Int, Int]): (Tentative[Literal], Tentative[Literal]) => Tentative[Literal] = {
    case (a1, a2) => intBox(intTag, Computation(f, Seq(intUnbox(a1), intUnbox(a2))))
  }
  def liftNBB(
      f: FunN[Boolean, Boolean]): (Tentative[Literal], Tentative[Literal]) => Tentative[Literal] = {
    case (a1, a2) => boolBox(Computation(f, Seq(boolUnbox(a1), boolUnbox(a2))))
  }

  val temporalOrigin = Cst(0)
  val temporalHorizon: Tentative[Int] = Input[Int]().subjectTo(temporalOrigin <= _)
  def anonymousTp(): Tentative[Int] =
    Input[Int]().subjectTo(tp => temporalOrigin <= tp && tp <= temporalHorizon)

  def encode(orig: core.Fluent)(implicit argRewrite: Arg => Tentative[Literal]): Fluent =
    Fluent(orig.template, orig.params.map(encode(_)))

  def encode(orig: common.Constant)(
      implicit argRewrite: Arg => Tentative[Literal]): Tentative[Literal] =
    ??? //TODO
//    Fluent(orig.template, orig.params.map(p => encode(p)(argRewrite)))

  def eqv(lhs: common.Term, rhs: common.Term)(
      implicit argRewrite: Arg => Tentative[Literal]): Tentative[Boolean] =
    eqv(encode(lhs), encode(rhs))

  private def isInt(e: Tentative[Literal]): Boolean = e.typ match {
    case t: BoxedInt[_] =>
      assert(t == intTag)
      true
    case t: TagIsoInt[_] =>
      assert(topTag.min <= t.min && t.max <= topTag.max)
      false
    case _ =>
      unexpected
  }
  def eqv(lhs: Tentative[Literal], rhs: Tentative[Literal]): Tentative[Boolean] =
    (lhs, rhs) match {
      case (Cst(x), Cst(y)) if x == y => bool.True
      case (Cst(x), Cst(y)) if x != y => bool.False
      case _ =>
        if(isInt(lhs) != isInt(rhs))
          bool.False
        else
          int.EQ(intUnbox(lhs), intUnbox(rhs))
    }

  def neq(lhs: common.Term, rhs: common.Term)(
      implicit argRewrite: Arg => Tentative[Literal]): Tentative[Boolean] =
    neq(encode(lhs), encode(rhs))
  def neq(lhs: Tentative[Literal], rhs: Tentative[Literal]): Tentative[Boolean] =
    not(eqv(lhs, rhs))

  def and(conjuncts: Tentative[Boolean]*): Tentative[Boolean] = {
    if(conjuncts.contains(bool.False))
      bool.False
    else {
      val unsatConjuncts = conjuncts.filter(_ != bool.True)
      if(unsatConjuncts.isEmpty)
        bool.True
      else
        Computation(bool.And, unsatConjuncts)
    }
  }
  def or(disjuncts: Tentative[Boolean]*): Tentative[Boolean] = {
    if(disjuncts.contains(bool.True))
      bool.True
    else {
      val satDisjuncts = disjuncts.filter(_ != bool.False)
      if(satDisjuncts.isEmpty)
        bool.False
      else
        Computation(bool.Or, satDisjuncts)
    }
  }
  def xor(disjuncts: Tentative[Boolean]*): Tentative[Boolean] = {
    val noFalse = disjuncts.filter(_ != bool.False)
    if(noFalse.isEmpty)
      bool.False
    else
      Computation(bool.XOr, noFalse)
  }
  def implies(cond: Tentative[Boolean], effect: Tentative[Boolean]): Tentative[Boolean] = {
    if(cond == bool.False)
      bool.True
    else
      or(not(cond), effect)
  }
  def not(pred: Tentative[Boolean]): Tentative[Boolean] = {
    if(pred == bool.True)
      bool.False
    else if(pred == bool.False)
      bool.True
    else
      Computation(bool.Not, pred)
  }

}

object ProblemContext {
  import Type._
  def extract(m: Seq[InModuleBlock])(implicit predef: Predef): ProblemContext = {
    val objectTypes = m.collect { case TypeDeclaration(t: ObjType) => t }
    val objectSubtypes = mutable.LinkedHashMap[ObjType, mutable.Set[ObjType]]()
    val instances = m
      .collect { case InstanceDeclaration(i) => i }
      .map { case i @ Instance(_, t) => (t, i) }
      .groupBy(_._1)
      .mapValues(vs => vs.map(_._2).sortBy(_.id.name))
      .mapValues(is => is.map(ObjLit))

    for(t <- objectTypes) {
      objectSubtypes.getOrElseUpdate(t, mutable.Set())
      t.parent match {
        case Some(parent) =>
          objectSubtypes.getOrElseUpdate(parent, mutable.Set()) += t
        case None =>
      }
    }
    val x = mutable.ArrayBuffer[ObjType]()

    def process(t: ObjType): Unit = {
      assert(!x.contains(t))
      x += t
      objectSubtypes(t).foreach(process)
    }
    process(ObjectTop)

    val tmp: List[(ObjLit, Int)] =
      x.toList
        .flatMap(t => instances.getOrElse(t, Seq()).toList)
        .zipWithIndex
    val fromIndex = tmp.map(_.swap).toMap
    val toIndex = tmp.toMap
    assert(toIndex.size == fromIndex.size)

    def tagOf(t: ObjType): TagIsoInt[ObjLit] = {
      def instancesOf(t: ObjType): Seq[ObjLit] =
        instances.getOrElse(t, Seq()) ++ objectSubtypes(t).flatMap(instancesOf)
      def continuousMinMax(is: Seq[ObjLit]): (Int, Int) = {
        val sorted = is.sortBy(t => toIndex(t)).toList
        sorted match {
          case Nil          => (0, -1)
          case head :: tail =>
            // assert that all instances have a continuous index space
            var curr = toIndex(head)
            for(next <- tail) {
              assert(toIndex(next) == curr + 1)
              curr = curr + 1
            }
            (toIndex(sorted.head), toIndex(sorted.last))
        }
      }
      val is = instancesOf(t)
      val (minId, maxId) = continuousMinMax(is)

      new TagIsoInt[ObjLit] {
        override def toInt(t: ObjLit): Int = {
          val ret = toIndex(t)
          assert(min <= ret && ret <= max)
          ret
        }

        override def fromInt(i: Int): ObjLit = {
          assert(min <= i && i <= max)
          fromIndex(i)
        }

        override val max: Int = maxId
        override val min: Int = minId

        override def typ: Tag.Type = Tag.typeOf[ObjLit]

        override def toString: String = s"${t.id}[$min,$max]"
      }
    }

    val topTag = new TagIsoInt[ObjLit] {
      override def toInt(t: ObjLit): Int = {
        val ret = toIndex(t)
        assert(min <= ret && ret <= max)
        ret
      }

      override def fromInt(i: Int): ObjLit = {
        assert(min <= i && i <= max)
        fromIndex(i)
      }

      override val min: Int = toIndex.values.min
      override val max: Int = toIndex.values.max

      override def typ: Tag.Type = Tag.typeOf[ObjLit]

      override def toString: String = s"TOP[$min,$max]"
    }
    val intTag = new BoxedInt[IntLit] {
      override def fromInt(i: Int): IntLit = IntLit(i)
      override def toInt(t: IntLit): Int = t.value

      override val min: Int = Int.MinValue / 3
      override val max: Int = Int.MaxValue / 3

      override def typ: Tag.Type = Tag.typeOf[IntLit]
    }

    val memo = mutable.Map[Type, TagIsoInt[ObjLit]]()
    val specializedTag: Type => TagIsoInt[_] = {
      case _: IIntType => intTag
      case t: ObjType  => memo.getOrElseUpdate(t, tagOf(t))
    }

    ProblemContext(intTag.asInstanceOf[BoxedInt[Literal]],
                   topTag.asInstanceOf[TagIsoInt[Literal]],
                   specializedTag.asInstanceOf[Type => TagIsoInt[Literal]])
  }
}

case class Chronicle(ctx: ProblemContext,
                     conditions: List[ConditionToken],
                     effects: List[EffectToken],
                     constraints: List[Tentative[Boolean]],
                     actions: List[Opt[Action[Tentative]]]) {

  import ctx._

  def extended(e: InActionBlock)(implicit argRewrite: Arg => Tentative[Literal]): Chronicle =
    e match {
      case _: LocalVarDeclaration => this
      case _: ArgDeclaration      => this
//      case BindAssertion(c, v) => //TODO
//        val cond = ConditionToken(
//          itv = ClosedInterval(ctx.temporalOrigin, ctx.temporalHorizon),
//          fluent = encode(c),
//          value = encode(v)
//        )
//        copy(
//          conditions = cond :: conditions
//        )

      case TimedAssignmentAssertion(itv, fluent, value) =>
        val changeItv = itv.map(encodeAsInt)
        val persistenceEnd = anonymousTp().subjectTo(changeItv.end <= _)
        val token = EffectToken(
          changeItv,
          persistenceEnd = persistenceEnd,
          fluent = encode(fluent),
          value = encode(value)
        )
        copy(effects = token :: effects)

      case TimedEqualAssertion(itv, f, v) =>
        val token = ConditionToken(
          itv = itv.map(encodeAsInt),
          fluent = encode(f),
          value = encode(v)
        )
        copy(conditions = token :: conditions)

      case TimedTransitionAssertion(ClosedInterval(s, e), f, v1, v2) =>
        val start = encodeAsInt(s)
        val changeEnd = encodeAsInt(e)
        val persistenceEnd = anonymousTp().subjectTo(changeEnd <= _)
        val cond = ConditionToken(ClosedInterval(start, start), encode(f), encode(v1))
        val eff =
          EffectToken(LeftOpenInterval(start, changeEnd), persistenceEnd, encode(f), encode(v2))
        copy(
          conditions = cond :: conditions,
          effects = eff :: effects
        )

      case StaticAssignmentAssertion(lhs, rhs) => ???
//        val eff = EffectToken(
//          changeItv = ClosedInterval(ctx.temporalOrigin, ctx.temporalOrigin),
//          persistenceEnd = ctx.temporalHorizon,
//          fluent = encode(lhs),
//          value = encode(rhs)
//        )
//        copy(
//          effects = eff :: effects
//        )
      case StaticBooleanAssertion(e) =>
        val c = ctx.boolUnbox(ctx.encode(e))
        copy(
          constraints = c :: constraints
        )
    }

  private def sameFluent(f1: Fluent, f2: Fluent): Tentative[Boolean] = {
    if(f1.template != f2.template)
      bool.False
    else {
      assert(f1.args.size == f2.args.size)
      val identical = f1.args.zip(f2.args).map { case (p1, p2) => ctx.eqv(p1, p2) }
      and(identical: _*)
    }
  }

  def toSatProblem = {
    var count = 0
    val acts: Seq[Opt[Action[Tentative]]] = actions
    val effs: Seq[Opt[EffectToken]] =
      effects.map(Opt.present) ++
        acts.flatMap(oa => oa.a.chronicle.effects.map(Opt(_, oa.present)))
    val conds: Seq[Opt[ConditionToken]] =
      conditions.map(Opt.present) ++
        acts.flatMap(oa => oa.a.chronicle.conditions.map(Opt(_, oa.present)))
    val consts =
      constraints ++
        acts.map(a => a.present.implies(and(a.a.chronicle.constraints: _*)))

    val nonOverlappingEffectsConstraints =
      for(e1 <- effs; e2 <- effs if e1 != e2) yield {
        count += 1
        (e1, e2) match {
          case (Opt(EffectToken(changeItv1, end1, fluent1, _), p1),
                Opt(EffectToken(changeItv2, end2, fluent2, _), p2)) =>
            implies(and(p1, p2, sameFluent(fluent1, fluent2)),
                    Interval.point(end1) < changeItv2 || Interval.point(end2) < changeItv1)
        }
      }

    val supportConstraints =
      conds.map {
        case Opt(ConditionToken(persItv, fc, vc), pc) =>
          val disjuncts = effs.map {
            case Opt(EffectToken(changeItv, persistenceEnd, fe, ve), pe) =>
              and(pe,
                  sameFluent(fc, fe),
                  ctx.eqv(vc, ve),
                  changeItv <= persItv,
                  persItv <= Interval.point(persistenceEnd))
          }
          implies(pc, or(disjuncts: _*))
      }

    val allConstraints = consts ++ nonOverlappingEffectsConstraints ++ supportConstraints

    val tmp = and(allConstraints: _*)

    val view = acts.map {
      case Opt(a, present) =>
        Product(
          Operator[Tentative](Cst(a.name)(Tag.default),
                              Product.fromSeq(a.args),
                              a.start,
                              a.end,
                              present))
    }

    SubjectTo(Product.fromSeq(view), and(allConstraints: _*))
  }
}

object Chronicle {
  def empty(ctx: ProblemContext): Chronicle =
    new Chronicle(ctx = ctx, conditions = Nil, effects = Nil, constraints = Nil, actions = Nil)
}

case class Opt[A](a: A, present: Tentative[Boolean]) {
  def map[B](f: A => B): Opt[B] = Opt(f(a), present)
}

object Opt {

  def present[A](a: A): Opt[A] = Opt(a, Cst(true))

  def optional[A](a: A): Opt[A] = Opt(a, Input[Boolean]())

}
