package dahu.planning.planner

import cats.Id
import dahu.model.functions.{->:, Fun1}
import dahu.model.input._
import dahu.model.products.FieldAccess
import dahu.model.types.{ProductTag, SequenceTag, Tag, TagIsoInt}
import dahu.model.input.dsl._
import dahu.model.math.{any, bool, sequence}
import dahu.planning.model.common.FunctionTemplate
import dahu.utils.Vec

class ContextProvider(t: TagIsoInt[Literal]) {
  implicit val literalTag: TagIsoInt[Literal] = t
  implicit val templateTag = Tag.default[FunctionTemplate]

  case class IntervalF[F[_]](start: F[Int], end: F[Int])
  type Interval = IntervalF[cats.Id]

  object IntervalF {

    def ofExpr(start: Expr[Int], end: Expr[Int]): Expr[Interval] =
      Product(IntervalF[Expr](start, end.subjectTo(_ >= start)))

    implicit val productTagInstance: ProductTag[IntervalF] = ProductTag[IntervalF]
    implicit val tagInstance: Tag[Interval] = productTagInstance

    val Start: FieldAccess[IntervalF, Int] = FieldAccess("start", 0)
    val End: FieldAccess[IntervalF, Int] = FieldAccess("end", 1)

    val Contains: Expr[Interval ->: Interval ->: Boolean] =
      Lambda(lhs => Lambda(rhs => Start(lhs) <= Start(rhs) && End(rhs) <= End(lhs)))

    val NonOverlapping: Expr[Interval ->: Interval ->: Boolean] =
      Lambda(lhs => Lambda(rhs => End(lhs) < Start(rhs) || End(rhs) < Start(lhs)))

    val Overlap: Expr[Interval ->: Interval ->: Boolean] =
      Lambda(lhs => Lambda(rhs => End(lhs) >= Start(rhs) && End(rhs) >= Start(lhs)))
  }

  case class FluentF[F[_]](template: F[FunctionTemplate], args: F[Vec[Literal]])
  type Fluent = FluentF[cats.Id]
  object FluentF {

    implicit val selfTag = ProductTag[FluentF]

//    val Template: FieldAccess[FluentF, FunctionTemplate] =
//      FieldAccess[FluentF, FunctionTemplate]("template", 0)
//    val Args = FieldAccess[FluentF, Vec[Literal]]("args", 1)
//
//    val unboxSeqLiteral: Fun1[Vec[Literal], Vec[Int]] = sequence.Map(t.unbox)
//    val EQX: Expr[Vec[Literal] ->: Vec[Literal] ->: Boolean] = Lambda(
//      lhs => Lambda(rhs => sequence.EQ(sequence.Map(t.unbox).apply(lhs), unboxSeqLiteral(rhs))))
//
//    val Equal: Expr[Fluent ->: Fluent ->: Boolean] = Lambda(
//      lhs => Lambda(rhs => EQX(Args(lhs), Args(rhs))))

  }

  case class CondTokF[F[_]](itv: F[Interval], fluent: F[Fluent], value: F[Literal])
  type CondTok = CondTokF[cats.Id]

  object CondTokF {

    implicit val productTag: ProductTag[CondTokF] = ProductTag[CondTokF]

    val Itv = FieldAccess[CondTokF, IntervalF[Id]]("itv", 0)
    val Fluent = FieldAccess[CondTokF, FluentF[Id]]("fluent", 1)
    val Value = FieldAccess[CondTokF, Literal]("value", 2)
  }

  case class EffTokF[F[_]](startChange: F[Int],
                           persistence: F[Interval],
                           fluent: F[Fluent],
                           value: F[Literal],
                           id: F[Int])
  type EffTok = EffTokF[cats.Id]

  object EffTokF {
    private var lastID: Int = 0
    def nextID(): Int = { lastID += 1; lastID }
    def ofExpr(startChange: Expr[Int],
               endChange: Expr[Int],
               fluent: Expr[Fluent],
               value: Expr[Literal]): EffTokF[Expr] = {
      val id = Cst(nextID())
      val endPersistence: Expr[Int] = Input[Int].subjectTo(_ >= endChange)
      new EffTokF[Expr](startChange.subjectTo(_ <= endChange),
                        IntervalF.ofExpr(endChange, endPersistence),
                        fluent,
                        value,
                        Cst(nextID()))
    }

    implicit val productTag: ProductTag[EffTokF] = ProductTag[EffTokF]

    val StartChange = FieldAccess[EffTokF, Int]("startChange", 0)
    val Persistence = FieldAccess[EffTokF, IntervalF[Id]]("persistence", 1)
    val Fluent = FieldAccess[EffTokF, FluentF[Id]]("fluent", 2)
    val Value = FieldAccess[EffTokF, Literal]("value", 3)
  }

  case class ChronicleF[F[_]](constraints: F[Boolean],
                              conditions: F[Vec[CondTok]],
                              effects: F[Vec[EffTok]]) {}
  object ChronicleF {

    val supportedBy: Expr[CondTok ->: EffTok ->: Boolean] =
      Lambda(cond =>
        Lambda(eff => {
          (any.EQ(CondTokF.Fluent(cond), EffTokF.Fluent(eff)): Expr[Boolean]) &&
          (any.EQ(CondTokF.Value(cond), EffTokF.Value(eff)): Expr[Boolean]) &&
          (IntervalF.Contains(EffTokF.Persistence(eff), CondTokF.Itv(cond)): Expr[Boolean])
        }))

    def supported: Expr[CondTok ->: Boolean] =
      Lambda(e => Dynamic[CondTok, EffTok, Boolean](e, supportedBy, bool.Or))

    def consistent(c: ChronicleF[Expr]): Expr[Boolean] =
      c.constraints && c.conditions
        .map(supported)
        .fold(bool.And) // sequence.Fold(bool.And).apply(MapSeq(c.conditions, supported))

  }

}
