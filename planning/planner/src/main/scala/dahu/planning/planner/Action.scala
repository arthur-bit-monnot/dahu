package dahu.planning.planner

import dahu.model.input.{Expr, Ident, Input, TypedIdent}
import dahu.planning.model.common._
import dahu.planning.model.core._
import dahu.planning.model.transforms.ActionInstantiation
import dahu.planning.planner.chronicles.Counter

case class Action[F[_]](name: String,
                        id: Int,
                        start: F[Int],
                        end: F[Int],
                        args: List[F[Literal]],
                        chronicle: Chronicle) {}

object Action {

  def instance(template: ActionTemplate, ctx: ProblemContext)(implicit predef: Predef,
                                                              cnt: Counter): Action[Expr] = {
    val id = cnt.next()
    val act = ActionInstantiation.instance(template, s"${template.name}_$id")
    val argsRewrite: Arg => Expr[Literal] = {
      case a @ Arg(_, tpe) => Input(TypedIdent(Ident(a), ctx.specializedTags(tpe)))
    }

    val chronicle = act.content.foldLeft(Chronicle.empty(ctx)) {
      case (c, s) => c.extended(s)(argsRewrite, cnt)
    }

    Action(
      act.template.name,
      id,
      ctx.intUnbox(ctx.encode(act.start)(argsRewrite)),
      ctx.intUnbox(ctx.encode(act.end)(argsRewrite)),
      act.args.toList.map(argsRewrite),
      chronicle
    )
  }
}
