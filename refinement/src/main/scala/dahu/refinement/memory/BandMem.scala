package dahu.refinement.memory
import dahu.model.products.ProductTagAny

case class BandMem(previousHappening: Happening,
                   firstState: State,
                   numStates: Int,
                   cstate: ProductTagAny) {
  def nextHappening: Happening = previousHappening.next
  def lastState: State = firstState + (numStates - 1)
  def states = firstState to lastState
  def state(i: Int) = firstState + i

}
