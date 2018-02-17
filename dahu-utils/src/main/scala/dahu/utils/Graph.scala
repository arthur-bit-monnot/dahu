package dahu.utils

import scala.collection.{immutable, mutable}

object Graph {

  /** Tarjan's algorithm for extracting strongly connected components.
    *
    * Given a directed graph, the algorithm outputs a sequence of strongly connected
    * components sorted in topological order.
    * */
  def tarjan[V](graph: Map[V, Set[V]]): Seq[Set[V]] = {
    class Data(var index: Int, var lowLink: Int, var onStack: Boolean)
    var index = 0
    val stack = new mutable.ArrayBuffer[V]()
    val data  = mutable.Map[V, Data]()

    var components: Seq[Set[V]] = immutable.Seq()

    for(v <- graph.keys) {
      if(!data.contains(v))
        strongConnect(v)
    }

    def strongConnect(v: V): Unit = {
      assert(!data.contains(v))
      data(v) = new Data(index, index, true)
      index += 1
      stack += v
      for(w <- graph.getOrElse(v, Set())) {
        if(!data.contains(w)) {
          strongConnect(w)
          data(v).lowLink = data(v).lowLink.min(data(w).lowLink)
        } else if(data(w).onStack) {
          data(v).lowLink = data(v).lowLink.min(data(w).index)
        }
      }

      if(data(v).lowLink == data(v).index) {
        var scc: Set[V] = Set()
        var w           = stack.last
        do {
          w = stack.last
          stack.remove(stack.size - 1)
          data(w).onStack = false
          scc += w
        } while(w != v)
        components = components :+ scc
      }
    }

    components.reverse
  }

  def topologicalOrder[V](graph: Map[V, Set[V]]): Option[Seq[V]] = {
    val sccs = tarjan(graph)

    sccs.foldLeft(Option(Seq[V]())) {
      case (opt, scc) if scc.size == 1 => opt.map(_ :+ scc.head)
      case _                           => None // non singleton strongly connected component
    }
  }

}
