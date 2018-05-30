import java.io.File

import dahu.planning.pddl.parser.PddlPredef
import dahu.planning.planner.chronicles.Planner
import spire.syntax.cfor

object Validation extends App {

  val domain = "/home/arthur/work/dahu/planning/rcll/simple.dom.pddl"
  val problem = "/home/arthur/work/dahu/planning/rcll/simple.01.pb.pddl"
//  val domain = "/home/arthur/work/dahu/planning/rcll/rcll_domain_production_durations.pddl"
//  val problem = "/home/arthur/work/dahu/planning/rcll/rcll.110.pb.pddl"

  class X {
    val pddlOptions = dahu.planning.pddl.parser.Options(discretization = 1000)
    val pddlParser = new dahu.planning.pddl.parser.Parser()(pddlOptions)
    implicit val predef: PddlPredef = pddlParser.predef

    def parse() = {
      pddlParser.parse(new File(domain), new File(problem))
    }

    def expr() = {
      Planner.asChronicleExpr(parse().get, _ => 1, true)
    }
  }
  val x1 = new X()
  val x2 = new X()
  assert(x1.parse() == x2.parse())
  val p1 = x1.expr()
  val p2 = x2.expr()
  println(p1)
  println(p2)
  val s1 = p1.toString
  val s2 = p2.toString
  cfor.cfor(0)(_ < s1.size, _ + 1) { i =>
    if(s1.charAt(i) != s2.charAt(i)) {
      val before = math.max(0, i - 20)
      val after = math.min(s1.size, i + 20)
      println(s1.substring(before, after))
      println(s2.substring(before, after))
    }
  }
  assert(p1 == p2)
}
