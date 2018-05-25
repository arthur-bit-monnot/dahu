package dahu.planning.planner.chronicles

import cats.Id
import dahu.model.functions.->:
import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.math.{any, bool}
import dahu.model.products.FieldAccess
import dahu.model.types.{ProductTag, Tag, TagIsoInt}
import dahu.planning.model.common.FunctionTemplate
import dahu.planning.planner.Literal
import dahu.utils.Vec

object DummyImplicits {
  // normally this is only supposed to be used for field access that will use the type of the underlying field anyway
  implicit val literalTag: Tag[Literal] = Tag.default[Literal]
  implicit val templateTag: Tag[FunctionTemplate] = Tag.default[FunctionTemplate]
}
import DummyImplicits._

case class IntervalF[F[_]](start: F[Int], end: F[Int])

object IntervalF {
  implicit val productTagInstance: ProductTag[IntervalF] = ProductTag.ofProd[IntervalF]
  implicit val tagInstance: Tag[Interval] = productTagInstance

  def ofExpr(start: Expr[Int], end: Expr[Int]): Expr[Interval] =
    Product(ofExprUnwrapped(start, end))

  def ofExprUnwrapped(start: Expr[Int], end: Expr[Int]): IntervalF[Expr] =
    IntervalF[Expr](start, end.subjectTo(_ >= start))

  val Start: FieldAccess[IntervalF, Int] = FieldAccess("start", 0)
  val End: FieldAccess[IntervalF, Int] = FieldAccess("end", 1)

  def contains(lhs: Expr[Interval], rhs: Expr[Interval]): Expr[Boolean] =
    Start(lhs) <= Start(rhs) && End(rhs) <= End(lhs)
//  val Contains: Expr[Interval ->: Interval ->: Boolean] =
//    Lambda(lhs => Lambda(rhs => Start(lhs) <= Start(rhs) && End(rhs) <= End(lhs)))

  val NonOverlapping: Expr[Interval ->: Interval ->: Boolean] =
    Lambda(lhs => Lambda(rhs => End(lhs) < Start(rhs) || End(rhs) < Start(lhs)))

  val Overlap: Expr[Interval ->: Interval ->: Boolean] =
    Lambda(lhs => Lambda(rhs => End(lhs) >= Start(rhs) && End(rhs) >= Start(lhs)))
}

case class FluentF[F[_]](template: F[FunctionTemplate], args: F[Vec[Literal]])

object FluentF {
  implicit val selfTag = ProductTag.ofProd[FluentF]

  def ofExpr(template: FunctionTemplate, args: Seq[Expr[Literal]]): Expr[Fluent] = {
    Product(FluentF[Expr](Cst(template), Sequence(Vec(args: _*))))
  }
}

case class CondTokF[F[_]](itv: F[Interval], fluent: F[Fluent], value: F[Literal])

object CondTokF {
  implicit val productTag: ProductTag[CondTokF] = ProductTag.ofProd[CondTokF]

  def ofExpr(start: Expr[Int],
             end: Expr[Int],
             fluent: Expr[Fluent],
             value: Expr[Literal]): Expr[CondTok] =
    Product(CondTokF[Expr](IntervalF.ofExpr(start, end), fluent, value))
      .subjectTo(i => Dynamic[CondTok, EffTok, Boolean](i, supportedBy, bool.Or))

  val Itv = FieldAccess[CondTokF, IntervalF[Id]]("itv", 0)
  val Fluent = FieldAccess[CondTokF, FluentF[Id]]("fluent", 1)
  val Value = FieldAccess[CondTokF, Literal]("value", 2)

  val supportedBy: Expr[CondTok ->: EffTok ->: Boolean] =
    Lambda(cond =>
      Lambda(eff => {
        (any.EQ(CondTokF.Fluent(cond), EffTokF.Fluent(eff)): Expr[Boolean]) &&
        (any.EQ(CondTokF.Value(cond), EffTokF.Value(eff)): Expr[Boolean]) &&
        (IntervalF.contains(EffTokF.Persistence(eff), CondTokF.Itv(cond)): Expr[Boolean])
      }))
}

case class EffTokF[F[_]](startChange: F[Int],
                         persistence: F[Interval],
                         fluent: F[Fluent],
                         value: F[Literal],
                         id: F[Int])

object EffTokF {
  implicit val productTag: ProductTag[EffTokF] = ProductTag.ofProd[EffTokF]

  private var lastID: Int = 0
  private def nextID(): Int = { lastID += 1; lastID }
  def ofExpr(startChange: Expr[Int],
             endChange: Expr[Int],
             endPersistenceOpt: Option[Expr[Int]],
             fluent: Expr[Fluent],
             value: Expr[Literal]): Expr[EffTok] = {
    val endPersistence: Expr[Int] =
      endPersistenceOpt.getOrElse(Input[Int]()).subjectTo(_ >= endChange)
    val tok = Product(
      new EffTokF[Expr](startChange.subjectTo(_ <= endChange),
                        IntervalF.ofExpr(endChange, endPersistence),
                        fluent,
                        value,
                        Cst(nextID())))
    DynamicProvider(tok, tok)
      .subjectTo(t => Dynamic[EffTok, EffTok, Boolean](t, NonThreatening, bool.And))
  }

  val StartChange = FieldAccess[EffTokF, Int]("startChange", 0)
  val Persistence = FieldAccess[EffTokF, IntervalF[Id]]("persistence", 1)
  val Fluent = FieldAccess[EffTokF, FluentF[Id]]("fluent", 2)
  val Value = FieldAccess[EffTokF, Literal]("value", 3)
  val Id = FieldAccess[EffTokF, Int]("id", 4)

  val NonThreatening: Expr[EffTok ->: EffTok ->: Boolean] = Lambda(
    lhs =>
      Lambda(
        rhs =>
          Id(lhs) >= Id(rhs) || //bool.True ||
            IntervalF.End(Persistence(rhs)) < StartChange(lhs) ||
            IntervalF.End(Persistence(lhs)) < StartChange(rhs) ||
            bool.Not(any.EQ(Fluent(lhs), Fluent(rhs)))))

}

case class ChronicleF[F[_]](constraints: F[Boolean],
                            conditions: F[Vec[CondTok]],
                            effects: F[Vec[EffTok]],
                            actions: F[Vec[Option[Action]]]) {}

object ChronicleF {
  implicit val tag: ProductTag[ChronicleF] = ProductTag.ofProd[ChronicleF]

  def ofExpr(constraints: Expr[Boolean],
             conditions: Expr[Vec[CondTok]],
             effects: Expr[Vec[EffTok]],
             actions: Expr[Vec[Option[Action]]]): Expr[Chronicle] = {
    val chron = ChronicleF(constraints, conditions, effects, actions)

    Product(chron).subjectTo(_ => constraints) // constraints on conditions and effects are directly placed on them
  }

}
