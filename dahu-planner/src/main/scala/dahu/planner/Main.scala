package dahu.planner

import copla.lang
import copla.lang.model.core.{ActionTemplate, Instance, Statement}
import dahu.utils.errors._
import java.io.File

import copla.lang.model.core

case class Config(problemFile: File = null)

object Main extends App {

  val parser = new scopt.OptionParser[Config]("dahu") {
    head("dahu", "0.x")
    arg[File]("XXXX.pb.anml").action((f, c) => c.copy(problemFile = f))
  }

  for(i <- 0 until 5)
    parser.parse(args, Config()) match {
      case Some(Config(pbFile)) =>
        solve(pbFile) match {
          case Some(sol) =>
            println("== Solution ==")
            println(sol)
          case None => unexpected
        }
      case None =>
    }

//  for(i <- Problems.satisfiables) {
//    solve(i) match {
//      case Some(sol) =>
//        println("== Solution ==")
//        println(sol)
//      case None => unexpected
//    }
//
//  }
//
//  for(i <- Problems.unsatisfiables) {
//    solve(i) match {
//      case Some(sol) => unexpected("Got solution on unsatisfiable problem.")
//      case None => println("No solution")
//    }
//  }

  def solve(problemFile: File): Option[Any] = {
    println("Parsing...")
    solve(lang.parse(problemFile))
  }
  def solve(anml: String): Option[Any] = {
    solve(lang.parse(anml))
  }

  def solve(parsed: lang.Result[core.CoreModel]): Option[Any] = {
    println("Encoding...")
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
