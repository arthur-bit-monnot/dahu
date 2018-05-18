package dahu.planning.planner

import dahu.model.input.{Expr, Ident, Input}
import dahu.planning.model.common._
import dahu.planning.model.core._
import dahu.planning.model.transforms.ActionInstantiation

case class Action[F[_]](name: String,
                        start: F[Int],
                        end: F[Int],
                        args: List[F[Literal]],
                        chronicle: Chronicle) {}

object Action {
  var counter = 0

  def instance(template: ActionTemplate, ctx: ProblemContext)(
      implicit predef: Predef): Action[Expr] = {
    counter += 1
    val act = ActionInstantiation.instance(template, s"${template.name}_$counter")
    val argsRewrite: Arg => Expr[Literal] = {
      case a @ Arg(_, tpe) => Input(Ident(a))(ctx.specializedTags(tpe))
    }

    val chronicle = act.content.foldLeft(Chronicle.empty(ctx)) {
      case (c, s) => c.extended(s)(argsRewrite)
    }

    Action(
      act.template.name,
      ctx.intUnbox(ctx.encode(act.start)(argsRewrite)),
      ctx.intUnbox(ctx.encode(act.end)(argsRewrite)),
      act.args.toList.map(argsRewrite),
      chronicle
    )
  }
}
