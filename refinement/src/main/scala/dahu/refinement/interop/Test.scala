package dahu.refinement.interop

import dahu.model.functions.->:
import dahu.model.input.{Cst, Expr, Lambda}
import dahu.model.problem.API

object Test extends App {
  import dahu.model.input.dsl._

  val dist: Expr[Double ->: Double] = Lambda(x => x - Cst(10.0))

  val parsed = API.parseAndProcess(dist, Nil)
  API.echo(parsed)

}
