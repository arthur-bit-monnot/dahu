package dahu.planning.pddl

import fr.uga.pddl4j.parser._

import scala.language.implicitConversions

object Main extends App {

  val domFile = "/home/arthur/work/fape/planning/domains/blocks_ipc2/pddl/blocks_ipc2.dom.pddl"
  val pbFile = "/home/arthur/work/fape/planning/domains/blocks_ipc2/pddl/blocks_ipc2.p04-0.pb.pddl"
//  val domFile =
//    "/home/arthur/work/ext/rcll/temporal_1_robot/rcll_domain_production_durations_nors.pddl"
//  val pbFile = "/home/arthur/work/ext/rcll/temporal_1_robot/problem-001-r1-o1-durations.pddl"
  val parser = new Parser()
  parser.parse(domFile, pbFile)

  val dom = parser.getDomain
  val pb = parser.getProblem

  println(dom)
//  println(pb)

  val factory = new ModelFactory(PddlPredef)
  factory.loadDomain(dom)
  factory.loadProblem(pb)

  println(factory.result)

}
