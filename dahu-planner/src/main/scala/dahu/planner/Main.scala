package dahu.planner

import copla.lang
import copla.lang.model.core.{ActionTemplate, Instance, Statement}
import dahu.utils.errors._
import java.io.File

import copla.lang.model.core
import dahu.model.input.Tentative

case class Config(problemFile: File = null,
                  encoding: Encoding = Incremental(20),
                  symBreak: Boolean = true,
                  useXorForSupport: Boolean = true)
sealed trait Encoding
case object Full extends Encoding
case class Incremental(maxSteps: Int) extends Encoding

object Main extends App {

  val parser = new scopt.OptionParser[Config]("dahu") {
    head("dahu", "0.x")
    arg[File]("XXXX.pb.anml").action((f, c) => c.copy(problemFile = f))
  }

  parser.parse(args, Config()) match {
    case Some(cfg) =>
      implicit val cfgImpl = cfg
      solve(cfg.problemFile) match {
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

  def solve(problemFile: File)(implicit cfg: Config): Option[Any] = {
    println("Parsing...")
    lang.parse(problemFile) match {
      case lang.Success(model) =>
        cfg.encoding match {
          case Full                  => solveFull(model)
          case Incremental(maxSteps) => solveIncremental(model, maxSteps)
        }
      case _ => sys.error("Parsing failed")
    }

  }
//  def solve(anml: String): Option[Any] = {
//    solve(lang.parse(anml))
//  }

  def solveFull(model: core.CoreModel)(implicit cfg: Config): Option[Any] = {
    println("Encoding...")
    val ctx = ProblemContext.extract(model)
    val result = model.foldLeft(Chronicle.empty(ctx)) {
      case (chronicle, statement: Statement) => chronicle.extended(statement)(_ => unexpected)
      case (chronicle, action: ActionTemplate) =>
        val argDomains: Seq[Set[Instance]] = action.args.map(a => {
          val tpe = ctx.specializedTags(a.typ)
          (tpe.min to tpe.max).toSet.map(tpe.fromInt)
        })
        val allParameterCombinations: Set[Array[Instance]] =
          dahu.utils.allCombinations(argDomains).map(_.toArray)
        val actionInstances = allParameterCombinations.map { args =>
          Opt.optional(Action.primitive(action, ctx)(args))
        }
        chronicle.copy(actions = chronicle.actions ++ actionInstances)
      case (chronicle, _) => chronicle
    }
//        println(result)
    val solution = Planner.solve(result)
//        println(solution)
    solution
  }

  def solveIncremental(model: core.CoreModel, maxSteps: Int)(implicit cfg: Config): Option[Any] = {
    for(i <- 0 to maxSteps) {
      println(s"Step: $i")
      solveIncrementalStep(model, i) match {
        case Some(sol) => return Some(sol)
        case None      =>
      }
    }
    None
  }

  def solveIncrementalStep(model: core.CoreModel, step: Int)(implicit cfg: Config): Option[Any] = {
    println("Encoding...")
    val ctx = ProblemContext.extract(model)
    val result = model.foldLeft(Chronicle.empty(ctx)) {
      case (chronicle, statement: Statement) => chronicle.extended(statement)(_ => unexpected)
      case (chronicle, action: ActionTemplate) =>
        val actionInstances: Seq[Opt[Action[Tentative]]] =
          if(cfg.symBreak) {
            import dahu.model.input.dsl._
            (0 until step).foldLeft(List[Opt[Action[Tentative]]]()) {
              case (Nil, _) => // first action
                Opt.optional(Action.instance(action, ctx)) :: Nil
              case (last :: rest, _) =>
                // not first, enforce that this action is only present if the last one is and that its start no earliest that the last one
                val act = Opt.optional(Action.instance(action, ctx))
                val presence = act.present implies last.present
                val after = act.a.start >= last.a.start
                val withSymBreak: Opt[Action[Tentative]] = act.copy(
                  a = act.a.copy(
                    chronicle = act.a.chronicle.copy(
                      constraints = presence :: after :: act.a.chronicle.constraints
                    )))
                withSymBreak :: last :: rest
            }

          } else {
            (0 until step).map { _ =>
              Opt.optional(Action.instance(action, ctx))
            }
          }
        chronicle.copy(actions = chronicle.actions ++ actionInstances)
      case (chronicle, _) => chronicle
    }
//        println(result)
    val solution = Planner.solve(result)
//        println(solution)
    solution
  }

}
