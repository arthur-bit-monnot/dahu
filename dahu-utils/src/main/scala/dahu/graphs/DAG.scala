package dahu.graphs

import dahu.utils.Graph

/** A Directed Acyclic Graph */
trait DAG[F[_], A] {

  /** Generation of a node from a node ID. */
  def algebra: A => F[A]

  /** All direct children of a node. */
  def children(graph: F[A]): Set[A]

  def descendants(a: A): Set[A] = children(algebra(a)).flatMap(descendants) + a

  def topologicalOrder(nodes: Set[A]): Seq[A] = {
    val graph = nodes.map(n => (n, children(algebra(n)))).toMap
    Graph.topologicalOrder(graph) match {
      case Some(order) => order
      case None        => dahu.utils.errors.unexpected("This dag instance is not a DAG.")
    }
  }
  def topologicalOrderFromRoot(a: A): Seq[A] = {
    topologicalOrder(descendants(a))

  }
}
