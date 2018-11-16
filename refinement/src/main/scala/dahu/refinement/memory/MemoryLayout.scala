package dahu.refinement.memory

import java.io.{BufferedWriter, File, FileWriter}

import dahu.model.products.{Field, ProductTagAny}
import dahu.utils.errors._
import dahu.refinement._
import dahu.refinement.interop.Printer

case class MemoryLayout(bands: IndexedSeq[BandMem], cstate: ProductTagAny) {

  require(bands.nonEmpty)
  require(bands.head.firstState == firstState.next)

  def firstState: State = new State(0)
  def lastState: State = bands.last.lastState.next

  def allStates = firstState to lastState

  private val dtOffset = cstate.fieldPosition("dt").getOrElse(unexpected("cstate has no dt field"))

  val firstStateAddress = 0
  val stateSize: Int = cstate.fields.size
  def addressOfState(s: State, fieldNum: Int = 0): Addr =
    firstStateAddress + (stateSize * s.i) + fieldNum
  def stateOfHappening(h: Happening): State = {
    val i = h.i
    if(i > 0)
      bands(i - 1).lastState.next
    else
      bands(i).firstState.previous

  }

  def addressOfTime(s: State): Int = addressOfState(s, dtOffset)

  def lastAddress: Addr = addressOfTime(allStates.last)

  def apply(b: Int) = bands(b)

  def print(mem: RMemory): Unit = {
    for((b, i) <- bands.zipWithIndex) {
      val h = new Happening(i)
      println(s"${printState(stateOfHappening(h), mem)}  ---   HAPPENING $i")
      for(s <- b.states) {
        println(printState(s, mem))
      }
      if(i == bands.size - 1)
        println(s"${printState(stateOfHappening(h.next), mem)}  ---   HAPPENING ${i + 1}")

    }
  }
  def printCSV(mem: RMemory, filename: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    def println(str: String): Unit = { bw.write(str); bw.newLine() }
    println(csvHeader())
    for((b, i) <- bands.zipWithIndex) {
      val h = new Happening(i)
      println(printStateCSV(s"h$i", stateOfHappening(h), mem))
      for(s <- b.states) {
        println(printStateCSV(s"b$i", s, mem))
      }
      if(i == bands.size - 1)
        println(printStateCSV(s"h${i + 1}", stateOfHappening(h.next), mem))

    }
    bw.close()
  }
  private val csvSeparator = ",\t"

  def csvHeader(withDeriv: Boolean = true): String = {
    def fmt(f: Field): Seq[String] = Seq(f.name, f.name + "'")
    val columns = "k" +: cstate.fields.toSeq
      .flatMap(fmt)
    columns.mkString(csvSeparator)
  }

  def printStateCSV(key: String, state: State, mem: RMemory, withDeriv: Boolean = true): String = {
    val t = (firstState to state).map(_.dt(this, mem)).sum
    def fmt(f: Field): Seq[String] = {
      val base = s"${Printer.fmt(mem.read(addressOfState(state, f.position)))}"
      val deriv = if(firstState < state) {
        val d = (mem.read(addressOfState(state, f.position)) - mem.read(
          addressOfState(state.previous, f.position))) /
          mem.read(addressOfTime(state))
        Printer.fmt(d)
      } else ""
      Seq(base, deriv)
    }
    val line = key +: Printer.fmt(t) +: cstate.fields.toSeq
      .flatMap(fmt)
    line.mkString(csvSeparator)
  }

  def printState(state: State, mem: RMemory, withDeriv: Boolean = true): String = {
    def fmt(f: Field): String = {
      val base = s"\t${f.name}: ${Printer.fmt(mem.read(addressOfState(state, f.position)))}"
      if(withDeriv && firstState < state) {
        val d = (mem.read(addressOfState(state, f.position)) - mem.read(
          addressOfState(state.previous, f.position))) /
          mem.read(addressOfTime(state))
        base + s"\t ${f.name}': ${Printer.fmt(d)}"
      } else base
    }
    s"${addressOfState(state)}:  " +
      cstate.fields
        .map(fmt)
        .mkString("")
  }

}
