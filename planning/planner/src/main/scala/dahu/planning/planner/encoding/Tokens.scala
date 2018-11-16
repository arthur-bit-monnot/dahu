package dahu.planning.planner.encoding

import cats.Id
import dahu.model.functions.{->:, Fun}
import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.math.{any, bool}
import dahu.model.products._
import dahu.model.types.Tag.Type
import dahu.model.types.{Bool, Tag}
import dahu.planning.model.common.{operators, FluentTemplate, FunctionTemplate}
import dahu.utils._
import dahu.utils.errors._
import spire.syntax.cfor

class Counter {
  private var n = 0
  def next(): Int = { n += 1; n - 1 }
}

object DummyImplicits {
  // normally this is only supposed to be used for field access that will use the type of the underlying field anyway
  implicit val literalTag: Tag[Literal] = Tag.default[Literal]
  implicit val templateTag: Tag[FunctionTemplate] = Tag.default[FunctionTemplate]
}
import dahu.planning.planner.encoding.DummyImplicits._

case class IntervalF[F[_]](start: F[Int], end: F[Int])

object IntervalF {
  implicit val productTagInstance: ProductTag[IntervalF] =
    ProductTag.build[IntervalF]("planning.interval", "start" -> Tag.ofInt, "end" -> Tag.ofInt)
  implicit val tagInstance: Tag[Interval] = productTagInstance

  def ofExpr(start: Expr[Int], end: Expr[Int]): Expr[Interval] =
    Product(ofExprUnwrapped(start, end))

  def ofExprUnwrapped(start: Expr[Int], end: Expr[Int]): IntervalF[Expr] =
    IntervalF[Expr](start, end)

  val Start: FieldAccess[IntervalF, Int] = productTagInstance.getAccessor[Int]("start")
  val End: FieldAccess[IntervalF, Int] = productTagInstance.getAccessor[Int]("end")

  def contains(lhs: Expr[Interval], rhs: Expr[Interval]): Expr[Bool] =
    Start(lhs) <= Start(rhs) && End(rhs) <= End(lhs)
}

case class FluentF[F[_]](template: F[FunctionTemplate], args: F[Vec[Literal]])

object FluentF {
  implicit val selfTag =
    ProductTag.build[FluentF]("planning.fluent",
                              "template" -> Tag.default[FunctionTemplate],
                              "args" -> Tag[Vec[Literal]])

  val Template = selfTag.getAccessor[FunctionTemplate]("template")

  def ofExpr(template: FunctionTemplate, args: Seq[Expr[Literal]]): Expr[Fluent] = {
    Product(FluentF[Expr](Cst(template), Sequence(Vec(args: _*))))
  }
}

case class CondTokF[F[_]](
    itv: F[Interval],
    fluent: F[Fluent],
    predicate: F[Literal ->: Bool],
    decisionLevel: F[Int],
    insertionLevel: F[Int],
    supportingAction: F[Int]
)

object CondTokF {
  implicit val productTag: ProductTag[CondTokF] = ProductTag.build[CondTokF](
    "planning.cond-tok",
    "itv" -> Tag[Interval],
    "fluent" -> Tag[Fluent],
    "predicate" -> Tag[Literal ->: Bool],
    "decision-level" -> Tag.ofInt,
    "insertion-level" -> Tag.ofInt,
    "supporting-action" -> Tag.ofInt
  )

  val Itv = productTag.getAccessor[Interval]("itv")
  val Fluent = productTag.getAccessor[Fluent]("fluent")
  val Predicate = productTag.getAccessor[Literal ->: Bool]("predicate")
  val DecLvl = productTag.getAccessor[Int]("decision-level")
  val InsLvl = productTag.getAccessor[Int]("insertion-level")
  val SupportingAction = productTag.getAccessor[Int]("supporting-action")

  import dahu.model.input.dsl._
  def ofExpr(start: Expr[Int],
             end: Expr[Int],
             fluent: Expr[Fluent],
             operator: operators.BinaryOperator,
             value: Expr[Literal],
             decLvl: Expr[Int],
             insLvl: Expr[Int],
             supportingAction: Expr[Int]): Expr[CondTok] = {

    fluent match {
      case Product(FluentF(Cst(f: FluentTemplate), _)) =>
        require(!f.isContinuous)
      case _ =>
    }

    val f: Expr[Literal] => Expr[Bool] = operator match {
      case operators.Eq =>
        x =>
          x ==== value
      case operators.LEQ =>
        x =>
          Computation2(dahu.model.math.any.LEQ, x, value)
      case op => unsupported(s"Operator $op is not supported yet")
    }

    val lbd = Lambda[Literal, Bool](f)

    Product(
      CondTokF[Expr](IntervalF.ofExpr(start, end), fluent, lbd, decLvl, insLvl, supportingAction))

  }

}

case class EffTokF[F[_]](startChange: F[Int],
                         persistence: F[Interval],
                         fluent: F[Fluent],
                         value: F[Literal],
                         id: F[Int],
                         insLvl: F[Int],
                         containingAction: F[Int])

object EffTokF {
  implicit val productTag: ProductTag[EffTokF] = ProductTag.build[EffTokF](
    "planning.eff-tok",
    "start-change" -> Tag.ofInt,
    "persistence" -> Tag[Interval],
    "fluent" -> Tag[Fluent],
    "value" -> Tag[Literal],
    "id" -> Tag.ofInt,
    "ins-lvl" -> Tag.ofInt,
    "containing-action" -> Tag.ofInt
  )

//  // TODO: support subtyping of tags
//  final case class EffProductTag(template: FunctionTemplate,
//                                 args: Vec[Option[Literal]],
//                                 value: Option[Literal],
//                                 id: Int)
//      extends ProductTag[EffTokF] {
//    override def exprProd: ProductExpr[EffTokF, Expr] = productTag.exprProd
//
//    override def idProd: ProductExpr[EffTokF, Id] = productTag.idProd
//
//    override def typ: Type = productTag.typ
//    override def clazz: ClassTag[EffTok] = implicitly[ClassTag[EffTok]]
//  }

  private var lastID: Int = 0
  private def nextID(): Int = { lastID += 1; lastID }
  def ofExpr(startChange: Expr[Int],
             endChange: Expr[Int],
             endPersistence: Expr[Int],
             fluent: Expr[Fluent],
             value: Expr[Literal],
             insLvl: Expr[Int],
             containingAction: Expr[Int])(implicit cnt: Counter): Expr[EffTok] = {
    val id = cnt.next()
//    val tag = fluent match {
//      case Product(FluentF(cf @ Cst(f), Sequence(args))) =>
//        EffProductTag(f, args.map {
//          case Cst(lit) => Some(lit)
//          case _        => None
//        }, value match {
//          case Cst(v) => Some(v)
//          case _      => None
//        }, id)
//      case _ => unexpected
//    }
    Product(
      new EffTokF[Expr](startChange,
                        IntervalF.ofExpr(endChange, endPersistence),
                        fluent,
                        value,
                        Cst(id),
                        insLvl,
                        containingAction))
//      .subjectTo(t => Dynamic[EffTok, Boolean](NonThreatening(t), bool.And, Some(Accept(tag))))
  }
  def compatible[A](a: Option[A], b: Option[A]): Boolean = (a, b) match {
    case (None, _)          => true
    case (_, None)          => true
    case (Some(l), Some(r)) => l == r
  }
  def compatibles[A](as: Vec[Option[A]], bs: Vec[Option[A]]): Boolean = {
    if(as.size != bs.size)
      return false
    val len = as.size
    cfor.cfor(0)(_ < len, _ + 1) { i =>
      if(!compatible(as(i), bs(i)))
        return false
    }
    true
  }
//  case class Accept(from: EffProductTag) extends (Tag[EffTok] => Boolean) {
//    override def apply(to: Tag[EffTok]): Boolean = to match {
//      case EffProductTag(t, args, value, id) =>
//        from.id < id &&
//          from.template == t &&
//          compatibles(from.args, args)
//      case _ => false
//    }
//  }

  val StartChange = productTag.getAccessor[Int]("start-change")
  val Persistence = productTag.getAccessor[IntervalF[Id]]("persistence")
  val Fluent = productTag.getAccessor[FluentF[Id]]("fluent")
  val Value = productTag.getAccessor[Literal]("value")
  val Id = productTag.getAccessor[Int]("id")
  val InsLvl = productTag.getAccessor[Int]("ins-lvl")
  val Container = productTag.getAccessor[Int]("containing-action")

  def consistent(lhs: Expr[EffTok], rhs: Expr[EffTok]): Expr[Bool] =
//    Id(lhs) >= Id(rhs) || // redundant with accept function
    IntervalF.End(Persistence(rhs)) < StartChange(lhs) ||
      IntervalF.End(Persistence(lhs)) < StartChange(rhs) ||
      bool.Not(any.EQ(Fluent(lhs), Fluent(rhs)))

  val nonThreatening: Expr[EffTok ->: EffTok ->: Bool] =
    Lambda[EffTok, EffTok ->: Bool](
      lhs =>
        Lambda[EffTok, Bool](
          (rhs: Expr[EffTok]) =>
            Id(lhs) >= Id(rhs) || // redundant with accept function
              IntervalF.End(Persistence(rhs)) < StartChange(lhs) ||
              IntervalF.End(Persistence(lhs)) < StartChange(rhs) ||
              bool.Not(any.EQ(Fluent(lhs), Fluent(rhs)))
      ))

  def NonThreatening(lhs: Expr[EffTok]): Expr[EffTok ->: Bool] = nonThreatening.partialApply(lhs)
//    Lambda[EffTok, Bool](
//      (rhs: Expr[EffTok]) =>
//        //Id(lhs) >= Id(rhs) || // superseded by accept function
//        IntervalF.End(Persistence(rhs)) < StartChange(lhs) ||
//          IntervalF.End(Persistence(lhs)) < StartChange(rhs) ||
//          bool.Not(any.EQ(Fluent(lhs), Fluent(rhs)))
//    )

}

case class ContCondTokF[F[_]](
    itv: F[Interval],
    predicate: F[Bool]
)

object ContCondTokF {
  implicit val fluentTemplateTag = Tag.default[FluentTemplate]
  implicit val productTag: ProductTag[ContCondTokF] = ProductTag.build[ContCondTokF](
    "cont-cond-tok",
    "itv" -> Tag[Interval],
    "predicate" -> Tag[Bool],
  )

  val Itv = productTag.getAccessor[Interval]("itv")
  val Predicate = productTag.getAccessor[Bool]("predicate")
//  val DecLvl = productTag.getAccessor[Int]("decision-level")
//  val InsLvl = productTag.getAccessor[Int]("insertion-level")
//  val SupportingAction = productTag.getAccessor[Int]("supporting-action")

  import dahu.model.input.dsl._
  def ofExpr(start: Expr[Int],
             end: Expr[Int],
             fluent: FluentTemplate,
             operator: operators.BinaryOperator,
             value: Expr[Literal],
  ): Expr[ContCondTokF[Id]] = {
    require(fluent.isContinuous)
    require(fluent.params.isEmpty)

    case class ReadContF[F[_]](state: F[Int], field: F[String])
    implicit val tag =
      ProductTag.build[ReadContF]("read-cont", "state" -> Tag.ofInt, "field" -> Tag.ofString)
    val s = Cst(0, Tag.ofInt)
    val field = Cst(fluent.id.name, Tag.ofString)
    val p = ReadContF[Expr](s, field)

    val lhs = Product(p)

    val e = operator match {
      case operators.Eq =>
        Computation2(dahu.model.math.any.EQ, lhs, value)
      case operators.LEQ =>
        Computation2(dahu.model.math.any.LEQ, lhs, value)
      case operators.GEQ =>
        Computation2(dahu.model.math.any.LEQ, value, lhs)

      case op => unsupported(s"Operator $op is not supported yet")
    }
    Product(ContCondTokF[Expr](IntervalF.ofExpr(start, end), e))

  }

}
