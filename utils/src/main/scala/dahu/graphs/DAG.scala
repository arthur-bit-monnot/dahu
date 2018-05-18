package dahu.graphs

import dahu.utils.Graph

import scala.reflect.ClassTag

/** A Directed Acyclic Graph.
  *
  * @tparam F Parametric node: a node is of type F[A]. Note that one can use cats.Id if node is a recursive type.
  * @tparam A Id of the Node
  */
trait DAG[F[_], A] {

  /** Generation of a node from a node ID. */
  def algebra: A => F[A]

  /** All direct children of a node. */
  def children(graph: F[A]): Iterable[A]

  def descendantsAndSelf(a: A): Set[A] = children(algebra(a)).toSet.flatMap(descendantsAndSelf) + a

  def topologicalOrder(nodes: Set[A])(implicit ct: ClassTag[A]): Seq[A] = {
    val graph = nodes.map(n => (n, children(algebra(n)).toSet)).toMap
    Graph.topologicalOrder(graph) match {
      case Some(order) => order
      case None        => dahu.utils.errors.unexpected("This dag instance is not a DAG.")
    }
  }
  def topologicalOrderFromRoot(a: A)(implicit ct: ClassTag[A]): Seq[A] = {
    topologicalOrder(descendantsAndSelf(a))

  }
}

object DAG {
  def apply[F[_], A](implicit instance: DAG[F, A]): DAG[F, A] = instance
}
