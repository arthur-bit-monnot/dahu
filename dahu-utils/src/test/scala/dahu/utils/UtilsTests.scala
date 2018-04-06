package dahu.utils

import Graph._
import utest._

object UtilsTests extends TestSuite {

  // graph with 2 SCC
  val g1: Map[Int, Set[Int]] = Map(1 -> Set(2, 3), 2 -> Set(1))

  // directed acyclic graphs
  val dag1: Map[Int, Set[Int]] = Map(1 -> Set(2), 2 -> Set(3), 3 -> Set(4))
  val dag2: Map[Int, Set[Int]] = Map(1 -> Set(2, 4), 2 -> Set(3, 4), 3 -> Set(4))

  def tests = Tests {
    "graph-utils" - {
      "tarjan" - {
        // simple converter function to help the comparison.
        def tj(g: Map[Int, Set[Int]]): Seq[Set[Int]] =
          tarjan(g).toList.map(_.toScalaSet())

        tj(g1) ==> Seq(Set(1, 2), Set(3))
        assert(tj(dag1) == (1 to 4).map(Set(_)))
        assert(tj(dag2) == (1 to 4).map(Set(_)))
      }

      "topological-order" - {
        assert(topologicalOrder(g1).isEmpty)
        assert(topologicalOrder(dag1).contains(1 to 4))
        assert(topologicalOrder(dag2).contains(1 to 4))
      }
    }

    "combinations" - {
      allCombinations(Seq(Set(1, 2), Set(3, 4)))      ==> Set(Seq(1, 3), Seq(1, 4), Seq(2, 3), Seq(2, 4))
      allCombinations(Seq(Set(1, 2, 3), Set[Int]()))  ==> Set()
      allCombinations(Seq())                          ==> Set(Seq())
      allCombinations(Seq(Set(1), Set(2), Set(3, 4))) ==> Set(Seq(1, 2, 3), Seq(1, 2, 4))
    }
  }
}
