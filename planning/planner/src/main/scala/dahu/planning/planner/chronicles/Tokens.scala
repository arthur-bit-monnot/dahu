package dahu.planning.planner.chronicles

import cats.Id
import dahu.model.functions.->:
import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.math.{any, bool}
import dahu.model.products.FieldAccess
import dahu.model.types.Tag.Type
import dahu.model.types.{ProductTag, Tag, TagIsoInt}
import dahu.planning.model.common
import dahu.planning.model.common.FunctionTemplate
import dahu.planning.planner.Literal
import dahu.utils.Vec
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

  case class Accept(func: FunctionTemplate, args: Vec[Option[Literal]], v: Option[Literal])
      extends (Tag[EffTok] => Boolean) {
    override def apply(v1: Tag[EffTok]): Boolean = v1 match {
      case EffTokF.EffProductTag(et, eargs, ev, _) =>
        func == et && EffTokF.compatibles(args, eargs) && EffTokF.compatible(v, ev)
      case _ => false
    }
  }

  def ofExpr(start: Expr[Int],
             end: Expr[Int],
             fluent: Expr[Fluent],
             value: Expr[Literal]): Expr[CondTok] = {
    val (func, args, v) = fluent match {
      case Product(FluentF(Cst(f), Sequence(args))) =>
        (f, args.map {
          case Cst(lit) => Some(lit)
          case _        => None
        }, value match {
          case Cst(v) => Some(v)
          case _      => None
        })
      case _ => ???
    }

    val accept = Accept(func, args, v)

    Product(CondTokF[Expr](IntervalF.ofExpr(start, end), fluent, value))
      .subjectTo(i => Dynamic[EffTok, Boolean](supportedBy(i), bool.Or, Some(accept)))

  }

  val Itv = FieldAccess[CondTokF, IntervalF[Id]]("itv", 0)
  val Fluent = FieldAccess[CondTokF, FluentF[Id]]("fluent", 1)
  val Value = FieldAccess[CondTokF, Literal]("value", 2)

  def supportedBy(cond: Expr[CondTok]): Expr[EffTok ->: Boolean] =
    Lambda[EffTok, Boolean](eff => {
      (any.EQ(CondTokF.Fluent(cond), EffTokF.Fluent(eff)): Expr[Boolean]) &&
      (any.EQ(CondTokF.Value(cond), EffTokF.Value(eff)): Expr[Boolean]) &&
      (IntervalF.contains(EffTokF.Persistence(eff), CondTokF.Itv(cond)): Expr[Boolean])
    }).named(s"{$cond}-supported-by")
}

case class EffTokF[F[_]](startChange: F[Int],
                         persistence: F[Interval],
                         fluent: F[Fluent],
                         value: F[Literal],
                         id: F[Int])

object EffTokF {
  implicit val productTag: ProductTag[EffTokF] = ProductTag.ofProd[EffTokF]

  final case class EffProductTag(template: FunctionTemplate,
                                 args: Vec[Option[Literal]],
                                 value: Option[Literal],
                                 id: Int)
      extends ProductTag[EffTokF] {
    override def exprProd: ProductExpr[EffTokF, Expr] = productTag.exprProd

    override def idProd: ProductExpr[EffTokF, Id] = productTag.idProd

    override def typ: Type = productTag.typ
  }

  private var lastID: Int = 0
  private def nextID(): Int = { lastID += 1; lastID }
  def ofExpr(startChange: Expr[Int],
             endChange: Expr[Int],
             endPersistenceOpt: Option[Expr[Int]],
             fluent: Expr[Fluent],
             value: Expr[Literal])(implicit cnt: Counter): Expr[EffTok] = {
    val id = cnt.next()
    val endPersistence: Expr[Int] =
      endPersistenceOpt.getOrElse(Input[Int](Ident("___" + cnt.next()))).subjectTo(_ >= endChange)
    val tag = fluent match {
      case Product(FluentF(cf @ Cst(f), Sequence(args))) =>
        EffProductTag(f, args.map {
          case Cst(lit) => Some(lit)
          case _        => None
        }, value match {
          case Cst(v) => Some(v)
          case _      => None
        }, id)
    }
    val tok =
      Product(
        new EffTokF[Expr](startChange.subjectTo(_ <= endChange),
                          IntervalF.ofExpr(endChange, endPersistence),
                          fluent,
                          value,
                          Cst(id)))(tag)
    DynamicProvider(tok, tok)
      .subjectTo(t => Dynamic[EffTok, Boolean](NonThreatening(t), bool.And, Some(Accept(tag))))
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
  case class Accept(from: EffProductTag) extends (Tag[EffTok] => Boolean) {
    override def apply(to: Tag[EffTok]): Boolean = to match {
      case EffProductTag(t, args, value, id) =>
        from.id < id &&
          from.template == t &&
          compatibles(from.args, args)
      case _ => false
    }
  }

  val StartChange = FieldAccess[EffTokF, Int]("startChange", 0)
  val Persistence = FieldAccess[EffTokF, IntervalF[Id]]("persistence", 1)
  val Fluent = FieldAccess[EffTokF, FluentF[Id]]("fluent", 2)
  val Value = FieldAccess[EffTokF, Literal]("value", 3)
  val Id = FieldAccess[EffTokF, Int]("id", 4)

  def NonThreatening(lhs: Expr[EffTok]): Expr[EffTok ->: Boolean] =
    Lambda[EffTok, Boolean](
      rhs =>
        //Id(lhs) >= Id(rhs) || // superseded by accept function
        IntervalF.End(Persistence(rhs)) < StartChange(lhs) ||
          IntervalF.End(Persistence(lhs)) < StartChange(rhs) ||
          bool.Not(any.EQ(Fluent(lhs), Fluent(rhs)))).named(s"{$lhs}-non-threatening")

}

case class ChronicleF[F[_]](constraints: F[Boolean],
                            conditions: F[Vec[CondTok]],
                            effects: F[Vec[EffTok]],
                            staticEffects: F[Vec[SEffTok]],
                            actions: F[Vec[Option[Action]]]) {}

object ChronicleF {
  implicit val tag: ProductTag[ChronicleF] = ProductTag.ofProd[ChronicleF]

  def ofExpr(constraints: Expr[Boolean],
             conditions: Expr[Vec[CondTok]],
             effects: Expr[Vec[EffTok]],
             staticEffects: Expr[Vec[SEffTok]],
             actions: Expr[Vec[Option[Action]]]): Expr[Chronicle] = {
    val chron = ChronicleF(constraints, conditions, effects, staticEffects, actions)

    Product(chron).subjectTo(_ => constraints) // constraints on conditions and effects are directly placed on them
  }

}
