package dahu.planning.planner.chronicles

import cats.Id
import dahu.model.functions.->:
import dahu.model.input._
import dahu.model.input.dsl._
import dahu.model.math._
import dahu.model.products.FieldAccess
import dahu.model.types._
import dahu.planning.model.common.FunctionTemplate
import dahu.planning.planner.Literal
import dahu.utils.Vec
import spire.syntax.cfor
import DummyImplicits._

case class SCondTokF[F[_]](fluent: F[Fluent], value: F[Literal])

object SCondTokF {
  implicit val productTag: ProductTag[CondTokF] = ProductTag.ofProd[CondTokF]

  def ofExpr(fluent: Expr[Fluent], value: Expr[Literal]): Expr[Boolean] = {
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

    val accept: Tag[SEffTok] => Boolean = {
      case SEffTokF.SEffProductTag(et, eargs, ev) =>
        func == et && SEffTokF.compatibles(args, eargs) && SEffTokF.compatible(v, ev)
      case _ => false
    }

    val condTok = Product(SCondTokF[Expr](fluent, value))
    Dynamic[SEffTok, Boolean](supportedBy(condTok), bool.Or, Some(accept))
  }

  val Fluent = FieldAccess[SCondTokF, FluentF[Id]]("fluent", 0)
  val Value = FieldAccess[SCondTokF, Literal]("value", 1)

  def supportedBy(cond: Expr[SCondTok]): Expr[SEffTok ->: Boolean] =
    Lambda[SEffTok, Boolean](eff => {
      (any.EQ(SCondTokF.Fluent(cond), SEffTokF.Fluent(eff)): Expr[Boolean]) &&
      (any.EQ(SCondTokF.Value(cond), SEffTokF.Value(eff)): Expr[Boolean])
    }).named(s"{$cond}-supported-by")
}

case class SEffTokF[F[_]](fluent: F[Fluent], value: F[Literal])

object SEffTokF {
  implicit val productTag: ProductTag[SEffTokF] = ProductTag.ofProd[SEffTokF]

  final case class SEffProductTag(template: FunctionTemplate,
                                  args: Vec[Option[Literal]],
                                  value: Option[Literal])
      extends ProductTag[SEffTokF] {
    override def exprProd: ProductExpr[SEffTokF, Expr] = productTag.exprProd

    override def idProd: ProductExpr[SEffTokF, Id] = productTag.idProd

    override def typ: Tag.Type = productTag.typ
  }

  def ofExpr(fluent: Expr[Fluent], value: Expr[Literal]): Expr[SEffTok] = {
    val tag = fluent match {
      case Product(FluentF(cf @ Cst(f), Sequence(args))) =>
        SEffProductTag(f, args.map {
          case Cst(lit) => Some(lit)
          case _        => None
        }, value match {
          case Cst(v) => Some(v)
          case _      => None
        })
    }
    val tok =
      Product(new SEffTokF[Expr](fluent, value))(tag)
    DynamicProvider(tok, tok)
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

  val Fluent = FieldAccess[SEffTokF, FluentF[Id]]("fluent", 0)
  val Value = FieldAccess[SEffTokF, Literal]("value", 1)
}
