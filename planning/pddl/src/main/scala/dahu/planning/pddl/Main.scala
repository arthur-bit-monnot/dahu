package dahu.planning.pddl

import java.io.File

import scala.language.implicitConversions
import scala.util.{Failure, Success}

object Main extends App {

  val domFile = new File(
    "/home/arthur/work/fape/planning/domains/blocks_ipc2/pddl/blocks_ipc2.dom.pddl")
  val pbFile = new File(
    "/home/arthur/work/fape/planning/domains/blocks_ipc2/pddl/blocks_ipc2.p04-0.pb.pddl")
//  val domFile =
//    "/home/arthur/work/ext/rcll/temporal_1_robot/rcll_domain_production_durations_nors.pddl"
//  val pbFile = "/home/arthur/work/ext/rcll/temporal_1_robot/problem-001-r1-o1-durations.pddl"
  new Parser(Options()).parseToFull(domFile, pbFile) match {
    case Success(model) => println(model)
    case Failure(e) =>
      e.printStackTrace()
      System.exit(1)
  }

}
