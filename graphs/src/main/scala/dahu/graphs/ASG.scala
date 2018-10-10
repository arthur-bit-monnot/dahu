package dahu.graphs
import dahu.utils.SubSubInt

/** Abstract Syntax Graph */
trait ASG[K, F[_], Opt[_]] {
  type ID <: IDTop

  sealed trait Marker

  def rootedAt(root: K): RootedASG[K, F, Opt] = RootedASG(root, this)

  def fixID: OpenASG[K, F, Opt, SubSubInt[IDTop, Marker]]

  def castIDTo[NewInternalID <: IDTop]: OpenASG[K, F, Opt, NewInternalID] =
    this.asInstanceOf[OpenASG[K, F, Opt, NewInternalID]]
}
