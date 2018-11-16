package dahu.refinement.memory

import dahu.refinement._

class State(val i: Int) extends AnyVal {
  def previous: State = this - 1
  def next: State = this + 1
  def +(off: Int): State = new State(i + off)
  def -(off: Int): State = new State(i - off)

  def <(s: State): Boolean = i < s.i
  def <=(s: State): Boolean = i <= s.i

  def to(lastState: State): Iterable[State] =
    (i to lastState.i).map(x => new State(x))

  def valueOfField(i: Int)(implicit memoryLayout: MemoryLayout, mem: RMemory): Double = {
    val addr = memoryLayout.addressOfState(this, i)
    mem.read(addr)
  }
  def dt(implicit memoryLayout: MemoryLayout, mem: RMemory): Double = {
    val addr = memoryLayout.addressOfTime(this)
    mem.read(addr)
  }
}
