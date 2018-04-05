package dahu.planner

import copla.lang.model.core
import copla.lang.model.core._
import dahu.model.functions.WrappedFunction
import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.math.{bool, int}
import dahu.model.types.{ProductTag, Tag, TagIsoInt}
import dahu.utils.errors._

import scala.collection.mutable

case class Fluent(template: FluentTemplate, args: Array[Tentative[Instance]])
case class Constant(template: ConstantTemplate, args: Array[Tentative[Instance]])

case class ConditionToken(start: Tentative[Int],
                          end: Tentative[Int],
                          fluent: Fluent,
                          value: Tentative[Instance])
case class EffectToken(changeStart: Tentative[Int],
                       changeEnd: Tentative[Int],
                       persistenceEnd: Tentative[Int],
                       fluent: Fluent,
                       value: Tentative[Instance])

case class EffectF[F[_]](cs: F[Int],
                         ce: F[Int],
                         pe: F[Int],
                         value: F[Instance],
                         f: FluentTemplate,
                         args: List[F[Instance]]) {
  override def toString: String = s"]$cs,$ce[ $f${args.mkString("(", ",", ")")} = $value"
}
object EffectF {
  implicit val productTag: ProductTag[EffectF] = new ProductTag[EffectF] {

    override def exprProd: ProductExpr[EffectF, Tentative] = new ProductExpr[EffectF, Tentative] {
      override def extractTerms(prod: EffectF[Tentative]): Seq[Tentative[Any]] =
        (Seq(prod.cs, prod.ce, prod.pe, prod.value, Cst(prod.f)(Tag.default)) ++ prod.args)
          .map(_.asInstanceOf[Tentative[Any]])
      override def buildFromTerms(terms: Seq[Tentative[Any]]): EffectF[Tentative] =
        terms.toList match {
          case cs :: ce :: pe :: value :: f :: args =>
            f match {
              case Cst(template: FluentTemplate) =>
                EffectF[Tentative](
                  cs.asInstanceOf[Tentative[Int]],
                  ce.asInstanceOf[Tentative[Int]],
                  pe.asInstanceOf[Tentative[Int]],
                  value.asInstanceOf[Tentative[Instance]],
                  template,
                  args.asInstanceOf[List[Tentative[Instance]]]
                )
              case _ => unexpected
            }
        }
    }

    override def idProd: ProductExpr[EffectF, cats.Id] = new ProductExpr[EffectF, cats.Id] {
      override def extractTerms(prod: EffectF[cats.Id]): Seq[cats.Id[Any]] =
        Seq(prod.cs, prod.ce, prod.pe, prod.value, prod.f) ++ prod.args

      override def buildFromTerms(terms: Seq[cats.Id[Any]]): EffectF[cats.Id] = terms.toList match {
        case (cs: Int) :: (ce: Int) :: (pe: Int) :: (value: Instance) :: (f: FluentTemplate) :: args =>
          EffectF[cats.Id](cs, ce, pe, value, f, args.asInstanceOf[List[Instance]])
      }
    }

    override def typ: Tag.Type = ???
  }
}

case class ProblemContext(topTag: TagIsoInt[Instance],
                          specializedTags: Type => TagIsoInt[Instance]) {

  private val Equality = WrappedFunction.wrap(int.EQ)(topTag, implicitly[TagIsoInt[Boolean]])

  def encode(v: Var)(implicit argRewrite: Arg => Tentative[Instance]): Tentative[Instance] =
    v match {
      case lv @ LocalVar(_, tpe) => Input(Ident(lv))(specializedTags(tpe))
      case i @ Instance(_, tpe)  => Cst(i)(specializedTags(tpe))
      case a: Arg                => argRewrite(a)
    }

  private val temporalOrigin = Cst(0)
  private val temporalHorizon = Input[Int]().subjectTo(temporalOrigin <= _)

  def encode(tp: TPRef): Tentative[Int] = tp match {
    case TPRef(id, 0) if id.toString == "start" => temporalOrigin // TODO: match by string....
    case TPRef(id, 0) if id.toString == "end"   => temporalHorizon // TODO: match by string....
    case TPRef(id, 0) =>
      Input[Int](Ident(id)).subjectTo(tp => temporalOrigin <= tp && tp <= temporalHorizon)
    case TPRef(id, delay) => encode(TPRef(id, 0)) + delay
  }
  def encode(orig: core.Fluent)(implicit argRewrite: Arg => Tentative[Instance]): Fluent =
    Fluent(orig.template, orig.params.map(encode(_)).toArray)
  def encode(orig: core.Constant)(implicit argRewrite: Arg => Tentative[Instance]): Constant =
    Constant(orig.template, orig.params.map(encode(_)).toArray)

  def eqv(lhs: Var, rhs: Var)(implicit argRewrite: Arg => Tentative[Instance]): Tentative[Boolean] =
    eqv(encode(lhs), encode(rhs))
  def eqv(lhs: Tentative[Instance], rhs: Tentative[Instance]): Tentative[Boolean] =
    Computation(Equality, lhs, rhs)

  def neq(lhs: Var, rhs: Var)(implicit argRewrite: Arg => Tentative[Instance]): Tentative[Boolean] =
    neq(encode(lhs), encode(rhs))
  def neq(lhs: Tentative[Instance], rhs: Tentative[Instance]): Tentative[Boolean] =
    Computation(bool.Not, eqv(lhs, rhs))

}

object ProblemContext {
  def extract(m: Seq[InModuleBlock]): ProblemContext = {
    val types = m.collect { case TypeDeclaration(t) => t }
    val subtypes = mutable.LinkedHashMap[Type, mutable.Set[Type]]()
    val instances = m
      .collect { case InstanceDeclaration(i) => i }
      .map { case i @ Instance(_, t) => (t, i) }
      .groupBy(_._1)
      .mapValues(vs => vs.map(_._2).sortBy(_.id.name))

    for(t <- types) {
      subtypes.getOrElseUpdate(t, mutable.Set())
      t.parent match {
        case Some(parent) =>
          subtypes.getOrElseUpdate(parent, mutable.Set()) += t
        case None =>
      }
    }
    val x = mutable.ArrayBuffer[Type]()
    val roots = types.collect { case t @ Type(_, None) => t }.toList

    def process(t: Type): Unit = {
      assert(!x.contains(t))
      x += t
      subtypes(t).foreach(process)
    }
    roots.foreach(process)

    val tmp = x.toList.flatMap(t => instances.getOrElse(t, Seq()).toList).zipWithIndex
    val fromIndex = tmp.map(_.swap).toMap
    val toIndex = tmp.toMap
    assert(toIndex.size == fromIndex.size)

    def tagOf(t: Type): TagIsoInt[Instance] = {
      def instancesOf(t: Type): Seq[Instance] =
        instances.getOrElse(t, Seq()) ++ subtypes(t).flatMap(instancesOf)
      def continuousMinMax(is: Seq[Instance]): (Int, Int) = {
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

      new TagIsoInt[Instance] {
        override def toInt(t: Instance): Int = {
          val ret = toIndex(t)
          assert(min <= ret && ret <= max)
          ret
        }

        override def fromInt(i: Int): Instance = {
          assert(min <= i && i <= max)
          fromIndex(i)
        }

        override val max: Int = maxId
        override val min: Int = minId

        override def typ: Tag.Type = ???

        override def toString: String = s"${t.id}[$min,$max]"
      }
    }

    val memo = mutable.Map[Type, TagIsoInt[Instance]]()
    val specializedTag = (t: Type) => memo.getOrElseUpdate(t, tagOf(t))
    val topTag = new TagIsoInt[Instance] {
      override def toInt(t: Instance): Int = {
        val ret = toIndex(t)
        assert(min <= ret && ret <= max)
        ret
      }

      override def fromInt(i: Int): Instance = {
        assert(min <= i && i <= max)
        fromIndex(i)
      }

      override val min: Int = toIndex.values.min
      override val max: Int = toIndex.values.max

      override def typ: Tag.Type = ???

      override def toString: String = s"TOP[$min,$max]"
    }

    ProblemContext(topTag, specializedTag)
  }
}

case class Chronicle(ctx: ProblemContext,
                     binds: Map[Constant, Tentative[Instance]],
                     conditions: List[ConditionToken],
                     effects: List[EffectToken],
                     constraints: List[Tentative[Boolean]],
                     actions: List[Action[Tentative]]) {

  import ctx._

  def extended(e: Statement)(implicit argRewrite: Arg => Tentative[Instance]): Chronicle = e match {
    case _: TypeDeclaration      => this
    case _: InstanceDeclaration  => this
    case _: TimepointDeclaration => this
    case _: FunctionDeclaration  => this
    case _: LocalVarDeclaration  => this
    case _: ArgDeclaration       => this
    case BindAssertion(c, v) =>
      binds.get(encode(c)) match {
        case Some(other) =>
          this.copy(
            constraints = ctx.eqv(encode(v), other) :: constraints
          )
        case None =>
          this.copy(
            binds = binds.updated(encode(c), encode(v))
          )
      }
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
      val persistenceEnd = Input[Int]().subjectTo(changeEnd <= _)
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
      val persistenceEnd = Input[Int]().subjectTo(changeEnd <= _)
      val cond = ConditionToken(start, start, encode(f), encode(v1))
      val eff = EffectToken(changeStart, changeEnd, persistenceEnd, encode(f), encode(v2))
      copy(
        conditions = cond :: conditions,
        effects = eff :: effects
      )
  }
  private val TRUE = Cst(true)
  private val FALSE = Cst(false)

  private def sameFluent(f1: Fluent, f2: Fluent): Tentative[Boolean] = {
    if(f1.template != f2.template)
      FALSE
    else {
      assert(f1.args.size == f2.args.size)
      val identical = f1.args.zip(f2.args).map { case (p1, p2) => ctx.eqv(p1, p2) }
      Computation(bool.And, identical)
    }
  }

  def toSatProblem = {
    val prod = Product.fromMap(binds)

    val acts: Seq[Opt[Action[Tentative]]] = actions.map(Opt.optional)
    val effs: Seq[Opt[EffectToken]] =
      effects.map(Opt.present) ++
        acts.flatMap(oa => oa.a.chronicle.effects.map(Opt(_, oa.present)))
    val conds: Seq[Opt[ConditionToken]] =
      conditions.map(Opt.present) ++
        acts.flatMap(oa => oa.a.chronicle.conditions.map(Opt(_, oa.present)))

    assert(acts.forall(_.a.chronicle.binds.isEmpty))

    val bindConstraints =
      for(b1 <- binds.keys; b2 <- binds.keys if b1 != b2) yield {
        (b1, b2) match {
          case (Constant(tpl1, vars1), Constant(tpl2, vars2)) if tpl1 == tpl2 =>
            assert(vars1.length == vars2.length)
            val identical = vars1.zip(vars2).map { case (v1, v2) => ctx.eqv(v1, v2) }
            Computation(bool.And, identical).implies(ctx.eqv(binds(b1), binds(b2)))
          case _ =>
            TRUE
        }
      }

    val nonOverlappingEffectsConstraints =
      for(e1 <- effs; e2 <- effs if e1 != e2) yield {
        (e1, e2) match {
          case (Opt(EffectToken(start1, _, end1, fluent1, _), p1),
                Opt(EffectToken(start2, _, end2, fluent2, _), p2)) =>
            (p1 && p2 && sameFluent(fluent1, fluent2))
              .implies(end1 < start2 || end2 < start1)
        }
      }

    val supportConstraints =
      conds.map {
        case Opt(ConditionToken(sc, ec, fc, vc), pc) =>
          val disjuncts = effs.map {
            case Opt(EffectToken(_, persistenceStart, persistenceEnd, fe, ve), pe) =>
              pe && sameFluent(fc, fe) && ctx.eqv(vc, ve) && persistenceStart <= sc && ec <= persistenceEnd
          }
          pc implies Computation(bool.Or, disjuncts)
      }

    val allConstraints = constraints ++ bindConstraints ++ nonOverlappingEffectsConstraints ++ supportConstraints

    val view = acts.map {
      case Opt(a, present) =>
        Product(
          Operator[Tentative](Cst(a.name)(Tag.default),
                              Product.fromSeq(a.args),
                              a.start,
                              a.end,
                              present))
    }

    SubjectTo(Product.fromSeq(view), Computation(bool.And, allConstraints))
  }
}

object Chronicle {
  def empty(ctx: ProblemContext): Chronicle =
    new Chronicle(ctx = ctx,
                  binds = Map(),
                  conditions = Nil,
                  effects = Nil,
                  constraints = Nil,
                  actions = Nil)
}

case class Opt[A](a: A, present: Tentative[Boolean]) {
  def map[B](f: A => B): Opt[B] = Opt(f(a), present)
}

object Opt {

  def present[A](a: A): Opt[A] = Opt(a, Cst(true))

  def optional[A](a: A): Opt[A] = Opt(a, Input[Boolean]())

}
