package dahu.planner

import copla.lang
import copla.lang.model.core.{ActionTemplate, Instance, Statement}
import dahu.utils.errors._

object Main extends App {

  val pb = """
      |type X;
      |type Y < X;
      |type Z < X;
      |type W < Y;
      |instance X x1, x2, x3;
      |
      |constant X v1;
      |constant X v2;
      |constant X v3;
      |
      |v1 != v2;
      |v2 != v3;
      |v3 != v1;
    """.stripMargin

  println(solve(pb))

  for(i <- Problems.satisfiables) {
    val sol = solve(i)
    println("== Solution ==")
    println(sol)
  }

  for(i <- Problems.unsatisfiables) {
    val sol = solve(i)
    println("== Solution (none expected) ==")
    println(sol)
  }

  def solve(anml: String): Option[Any] = {
    println("\n\n=== Problem ===")
    println(anml)
    val parsed = lang.parse(anml)
    parsed match {
      case lang.Success(res) =>
        val ctx = ProblemContext.extract(res)
        val result = res.foldLeft(Chronicle.empty(ctx)) {
          case (chronicle, statement: Statement) => chronicle.extended(statement)(_ => unexpected)
          case (chronicle, action: ActionTemplate) =>
            val argDomains: Seq[Set[Instance]] = action.args.map(a => {
              val tpe = ctx.specializedTags(a.typ)
              (tpe.min to tpe.max).toSet.map(tpe.fromInt)
            })
            val allParameterCombinations: Set[Array[Instance]] =
              dahu.utils.allCombinations(argDomains).map(_.toArray)
            val actionInstances = allParameterCombinations.map { args =>
              Action.primitive(action, ctx)(args)
            }
            chronicle.copy(actions = chronicle.actions ++ actionInstances)
          case (chronicle, _) => chronicle
        }
//        println(result)
        val solution = Planner.solve(result)
//        println(solution)
        solution
      case x => sys.error(s"Parsing failed: $x")
    }
  }

}
