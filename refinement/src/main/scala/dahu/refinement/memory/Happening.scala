package dahu.refinement.memory

class Happening(val i: Int) extends AnyVal {
  def previous: Happening = this - 1
  def next: Happening = this + 1
  def +(off: Int): Happening = new Happening(i + off)
  def -(off: Int): Happening = new Happening(i - off)

  def <(s: Happening): Boolean = i < s.i
  def <=(s: Happening): Boolean = i <= s.i

  def to(lastHappening: Happening): Iterable[Happening] =
    (i to lastHappening.i).map(x => new Happening(x))
}
