package dahu.planning.planner.hcsp

class Counter {
  private var n = 0
  def next(): Int = { n += 1; n - 1 }
}
