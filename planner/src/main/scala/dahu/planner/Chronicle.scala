package dahu.planner

import copla.lang.model.common
import copla.lang.model.common.{Cst => _, Term => _, _}
import copla.lang.model.core._
import copla.lang.model.core
import dahu.model.functions.WrappedFunction
import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.math.{bool, int}
import dahu.model.types.{BoxedInt, Tag, TagIsoInt}
import dahu.utils.errors._

import scala.collection.mutable

case class Fluent(template: FunctionTemplate, args: Seq[Tentative[Literal]])
case class Constant(template: ConstantTemplate, args: Seq[Tentative[Literal]])

case class ConditionToken(start: Tentative[Int],
                          end: Tentative[Int],
                          fluent: Fluent,
                          value: Tentative[Literal])
case class EffectToken(changeStart: Tentative[Int],
                       changeEnd: Tentative[Int],
                       persistenceEnd: Tentative[Int],
                       fluent: Fluent,
                       value: Tentative[Literal])

sealed trait Literal {
  def asConstant(tag: TagIsoInt[Literal]): Cst[Literal] = Cst(this)(tag)
}
case class IntLit(value: Int) extends Literal {
  override def toString: String = value.toString
}
case class ObjLit(value: Instance) extends Literal {
  override def toString: String = value.toString
}

case class ProblemContext(intTag: BoxedInt[Literal],
                          topTag: TagIsoInt[Literal],
                          specializedTags: Type => TagIsoInt[Literal]) {

  def encode(v: common.Term)(implicit argRewrite: Arg => Tentative[Literal]): Tentative[Literal] =
    v match {
      case IntLiteral(i)         => IntLit(i).asConstant(intTag)
      case lv @ LocalVar(_, tpe) => Input[Literal](Ident(lv))(specializedTags(tpe))
      case i @ Instance(_, tpe)  => ObjLit(i).asConstant(specializedTags(tpe))
      case a: Arg                => argRewrite(a)
    }

  val temporalOrigin = Cst(0)
  val temporalHorizon: Tentative[Int] = Input[Int]().subjectTo(temporalOrigin <= _)
  def anonymousTp(): Tentative[Int] =
    Input[Int]().subjectTo(tp => temporalOrigin <= tp && tp <= temporalHorizon)

  def encode(ie: IntExpr)(implicit argRewrite: Arg => Tentative[Literal]): Tentative[Int] =
    ie match {
      case VarIntExpr(IntLiteral(d)) => Cst(d)
      case VarIntExpr(v: Var) =>
        assert(v.typ == Type.Integers)
        val variable = encode(v)
        variable.typ match {
          case tpe: BoxedInt[Literal] => variable.unboxed(tpe)
          case _                      => unexpected
        }
      case Add(lhs, rhs) => encode(lhs) + encode(rhs)
      case Minus(x)      => -encode(x)
      case x             => unexpected(s"Unsupported int expression: $x")
    }

  def encode(tp: TPRef)(implicit argRewrite: Arg => Tentative[Literal]): Tentative[Int] = tp match {
    case TPRef(id, VarIntExpr(IntLiteral(0))) if id.toString == "start" =>
      temporalOrigin // TODO: match by string....
    case TPRef(id, VarIntExpr(IntLiteral(0))) if id.toString == "end" =>
      temporalHorizon // TODO: match by string....
    case TPRef(id, VarIntExpr(IntLiteral(0))) =>
      Input[Int](Ident(id)).subjectTo(tp => temporalOrigin <= tp && tp <= temporalHorizon)
    case TPRef(id, delay) => encode(TPRef(id)) + encode(delay)
  }
  def encode(orig: core.Fluent)(implicit argRewrite: Arg => Tentative[Literal]): Fluent =
    Fluent(orig.template, orig.params.map(encode(_)))

  def encode(orig: core.Constant)(implicit argRewrite: Arg => Tentative[Literal]): Fluent =
    Fluent(orig.template, orig.params.map(p => encode(p)(argRewrite)))

  def eqv(lhs: common.Term, rhs: common.Term)(
      implicit argRewrite: Arg => Tentative[Literal]): Tentative[Boolean] =
    eqv(encode(lhs), encode(rhs))

  private val ObjEquality = WrappedFunction.wrap(int.EQ)(topTag, implicitly[TagIsoInt[Boolean]])
  private val IntEquality = WrappedFunction.wrap(int.EQ)(intTag, implicitly[TagIsoInt[Boolean]])
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
        if(isInt(lhs) && isInt(rhs))
          Computation(IntEquality, lhs, rhs)
        else if(isInt(lhs) || isInt(rhs))
          bool.False
        else {
          Computation(ObjEquality, lhs, rhs)
        }
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
  def extract(m: Seq[InModuleBlock]): ProblemContext = {
    val objectTypes = m.collect { case TypeDeclaration(t) if t != Type.Integers => t }
    val objectSubtypes = mutable.LinkedHashMap[Type, mutable.Set[Type]]()
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
    val x = mutable.ArrayBuffer[Type]()
    val roots = objectTypes.collect { case t @ Type(_, None) => t }.toList

    def process(t: Type): Unit = {
      assert(!x.contains(t))
      x += t
      objectSubtypes(t).foreach(process)
    }
    roots.foreach(process)

    val tmp: List[(ObjLit, Int)] =
      x.toList
        .flatMap(t => instances.getOrElse(t, Seq()).toList)
        .zipWithIndex
    val fromIndex = tmp.map(_.swap).toMap
    val toIndex = tmp.toMap
    assert(toIndex.size == fromIndex.size)

    def tagOf(t: Type): TagIsoInt[ObjLit] = {
      assert(t != Type.Integers)

      def instancesOf(t: Type): Seq[ObjLit] =
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

      override val min: Int = Int.MinValue / 2
      override val max: Int = Int.MaxValue / 2

      override def typ: Tag.Type = Tag.typeOf[IntLit]
    }

    val memo = mutable.Map[Type, TagIsoInt[ObjLit]]()
    val specializedTag = (t: Type) =>
      if(t == Type.Integers)
        intTag
      else
        memo.getOrElseUpdate(t, tagOf(t))

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
      case _: TypeDeclaration      => this
      case _: InstanceDeclaration  => this
      case _: TimepointDeclaration => this
      case _: FunctionDeclaration  => this
      case _: LocalVarDeclaration  => this
      case _: ArgDeclaration       => this
      case BindAssertion(c, v) =>
        val cond = ConditionToken(
          start = ctx.temporalOrigin,
          end = ctx.temporalHorizon,
          fluent = encode(c),
          value = encode(v)
        )
        copy(
          conditions = cond :: conditions
        )
      case StaticDifferentAssertion(lhs, rhs) =>
        copy(
          constraints = ctx.neq(lhs, rhs) :: constraints
        )
      case StaticEqualAssertion(lhs, rhs) =>
        copy(constraints = ctx.eqv(lhs, rhs) :: constraints)
      case TBefore(lhs, rhs) =>
        copy(constraints = (encode(lhs) <= encode(rhs)) :: constraints)

      case TimedAssignmentAssertion(start, end, fluent, value) =>
        val changeStart = encode(start)
        val changeEnd = encode(end).subjectTo(changeStart <= _)
        val persistenceEnd = anonymousTp().subjectTo(changeEnd <= _)
        val token = EffectToken(
          changeStart = changeStart,
          changeEnd = changeEnd,
          persistenceEnd = persistenceEnd,
          fluent = encode(fluent),
          value = encode(value)
        )
        copy(effects = token :: effects)

      case TimedEqualAssertion(s, e, f, v) =>
        val start = encode(s)
        val end = encode(e)
        val token = ConditionToken(
          start = start,
          end = end,
          fluent = encode(f),
          value = encode(v)
        )
        copy(conditions = token :: conditions)

      case TimedTransitionAssertion(s, e, f, v1, v2) =>
        val start = encode(s)
        val changeStart = start + 1
        val changeEnd = encode(e).subjectTo(changeStart <= _)
        val persistenceEnd = anonymousTp().subjectTo(changeEnd <= _)
        val cond = ConditionToken(start, start, encode(f), encode(v1))
        val eff = EffectToken(changeStart, changeEnd, persistenceEnd, encode(f), encode(v2))
        copy(
          conditions = cond :: conditions,
          effects = eff :: effects
        )

      case StaticAssignmentAssertion(lhs, rhs) =>
        val eff = EffectToken(
          changeStart = ctx.temporalOrigin,
          changeEnd = ctx.temporalOrigin,
          persistenceEnd = ctx.temporalHorizon,
          fluent = encode(lhs),
          value = encode(rhs)
        )
        copy(
          effects = eff :: effects
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

  def toSatProblem(implicit cfg: Config) = {
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
          case (Opt(EffectToken(start1, _, end1, fluent1, _), p1),
                Opt(EffectToken(start2, _, end2, fluent2, _), p2)) =>
            implies(and(p1, p2, sameFluent(fluent1, fluent2)), end1 < start2 || end2 < start1)
        }
      }

    val supportConstraints =
      conds.map {
        case Opt(ConditionToken(sc, ec, fc, vc), pc) =>
          val disjuncts = effs.map {
            case Opt(EffectToken(_, persistenceStart, persistenceEnd, fe, ve), pe) =>
              and(pe,
                  sameFluent(fc, fe),
                  ctx.eqv(vc, ve),
                  persistenceStart <= sc,
                  ec <= persistenceEnd)
          }
          if(cfg.useXorForSupport)
            implies(pc, xor(disjuncts: _*))
          else
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
