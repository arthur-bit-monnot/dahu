package dahu.planning.planner.chronicles

import dahu.model.input._
import dahu.model.products.FieldAccess
import dahu.model.types.{ProductTag, Tag}
import dahu.planning.model.common.{Arg, Predef}
import dahu.planning.model.core.ActionTemplate
import dahu.planning.model.transforms.ActionInstantiation
import dahu.planning.planner.{Literal, ProblemContext}
import dahu.utils.Vec

case class ActionF[F[_]](name: F[String],
                         start: F[Int],
                         end: F[Int],
                         args: F[Vec[Literal]],
                         chronicle: F[Chronicle]) {}

object ActionF {
  private implicit val literalTag: Tag[Literal] = Tag.default[Literal]
  implicit val tag: ProductTag[ActionF] = ProductTag.ofProd[ActionF]

  val Start = FieldAccess[ActionF, Int]("start", 1)
  val End = FieldAccess[ActionF, Int]("end", 2)

  def instance(template: ActionTemplate, ctx: ProblemContext)(implicit predef: Predef,
                                                              cnt: Counter): ActionF[Expr] = {
    val actId = cnt.next()
    val act = ActionInstantiation.instance(template, s"${template.name}_$actId")
    val argsRewrite: Arg => Expr[Literal] = {
      case a @ Arg(_, tpe) => Input(Ident(a))(ctx.specializedTags(tpe))
    }

    val chronicle = act.content.foldLeft(ChronicleFactory.empty(ctx)) {
      case (c, s) => c.extended(s)(argsRewrite, cnt)
    }

    ActionF(
      Cst(act.template.name),
      ctx.intUnbox(ctx.encode(act.start)(argsRewrite)),
      ctx.intUnbox(ctx.encode(act.end)(argsRewrite)),
      Sequence(act.args.map(argsRewrite)),
      chronicle.compile
    )
  }

  def optionalInstance(template: ActionTemplate,
                       ctx: ProblemContext)(implicit predef: Predef, cnt: Counter): Expr[Action] = {
    val act = instance(template, ctx)
    Optional(Product(act), Input[Boolean](Ident(act.name + "_prez?" + cnt.next())))
  }
}
