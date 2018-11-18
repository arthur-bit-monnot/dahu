package dahu.refinement.memory

import java.io.{BufferedWriter, File, FileWriter}

import dahu.model.products.{Field, ProductTagAny}
import dahu.refinement._
import dahu.refinement.interop.{Params, Printer}
import dahu.utils.errors._

class Mem(ioMem: RWMemory, val layout: MemoryLayout) {
  implicit private val memoryLayout = layout

  def memoryView: RMemory = ioMem
  def memoryRW: RWMemory = ioMem

  def print(): Unit = layout.print(ioMem)
  def printCSV(filename: String): Unit = layout.printCSV(ioMem, filename)

  def homogenize(): Unit = {
    layout.bands.foreach(homogenizeInPlace)
  }

  def homogenizeInPlace(b: BandMem): Unit =
    homogenizeInPlace(b.previousHappening, b.nextHappening)

  def homogenizeInPlace(start: Happening, end: Happening): Unit = {

    implicit val memory: RMemory = ioMem.copy

    val states = (memoryLayout.stateOfHappening(start).next to memoryLayout
      .stateOfHappening(end)
      .previous).toSeq
    val times = states.drop(1).foldLeft(Seq(0.0)) { case (l, s) => l :+ (l.last + s.dt) }
    val numDts = states.size - 1
    val totalTime = times.last
    val dt = totalTime / numDts
    val newTimes = states.toSeq.indices.map(_ * dt)

    var nextBefore = 0

    for(i <- 1 until states.size - 1) {
      val targetT = newTimes(i)
//      println(targetT)
//      println(times)
      while(!(times(nextBefore + 1) >= targetT)) {
        assert(nextBefore < times.size - 1)
        nextBefore += 1
      }
      val before = nextBefore
//        states.indices.dropRight(1).find(i => times(i) <= targetT && targetT <= times(i + 1)).get
      assert(before < states.size - 1)
      assert(times(before) <= targetT)
      assert(targetT <= times(before + 1))
      val beforeValue = states(before).valueOfField(0)
      val afterValue = states(before + 1).valueOfField(0)
      val originalDelta = times(before + 1) - times(before)
      val proportionBase = (targetT - times(before)) / originalDelta
      val proportion =
        if(proportionBase >= 0 && proportionBase <= 1)
          proportionBase
        else if(proportionBase < 0 && proportionBase > -1e-07)
          0.0
        else if(proportionBase > 1 && proportionBase < 1.0 + 1e-07)
          1.0
        else
          ???

      assert(proportion >= 0 && proportion <= 1)
      val newValue = beforeValue + (afterValue - beforeValue) * proportion

      if(newValue.isNaN) {
        print()
        new Mem(memory.asInstanceOf[RWMemory], layout).print()
      }

      val addr0 = memoryLayout.addressOfState(states(i))
      ioMem.write(addr0, newValue)
    }
    for(s <- states.tail) { // do not update DT of first happening
      val addrDt = memoryLayout.addressOfTime(s)
      ioMem.write(addrDt, dt)
    }
  }

  def averageDt(band: BandMem): Double = {
    val startState = band.firstState
    val endState = band.lastState.next
    val states = startState to endState
    val totalTime = states.map(_.dt(memoryLayout, ioMem)).sum
    val numDts = states.size
    totalTime / numDts
  }

  def grownLayout(targetDt: Double)(implicit params: Params): Option[MemoryLayout] = {
    val statesForBand: Int => Int = i => {
      val band = layout.bands(i)
      val dt = averageDt(band)
      val factor = dt / targetDt
      val numStates: Int = ((band.numStates + 1) * factor).floor.toInt
      math.min(
        math.max(numStates, params.statesPerBand),
        params.maxStatePerBand
      )

    }
    grownLayout(statesForBand)
  }

  /** Builds a new memory layout in whihc the number of states for band(i) is computed with statesForBand(i)
    *
    * Returns None if the layout is identical to the current one.
    * */
  def grownLayout(statesForBand: Int => Int): Option[MemoryLayout] = {

    val cstate = memoryLayout.cstate

    if(memoryLayout.bands.indices.forall(i => memoryLayout(i).numStates == statesForBand(i))) {
      return None
    }

    val newLayout = MemoryLayout(
      memoryLayout.bands.indices
        .foldLeft(List[BandMem]()) {
          case (l, i) =>
            val numStates: Int = statesForBand(i)
            val firstState = l.headOption
              .map(m => m.lastState.next.next) // use last state from previous band
              .getOrElse(new State(0).next)
            BandMem(new Happening(i), firstState, numStates, cstate) :: l
        }
        .reverse
        .toIndexedSeq,
      cstate
    )
    Some(newLayout)
  }

  def extendToMaxDiff(v: Double): Option[Mem] = {
    val newSize: Int => Int = i => {
      val band = layout.bands(i)
      val diffs = for(i <- 0 until layout.stateSize) yield {
        val v1 = band.firstState.previous.valueOfField(i)(memoryLayout, memoryRW)
        val v2 = band.lastState.next.valueOfField(i)(memoryLayout, memoryRW)
        v2 - v1
      }
      (diffs.max / v).toInt + 1
    }

    grownLayout(newSize).map(doubleIgnoreTime)
  }

  def doubleTimeBased(newLayout: MemoryLayout): Mem = {
    implicit val memory: RMemory = ioMem
    val res = Mem.makeFromLayout(newLayout)
    val targetMem: WMemory = res.memoryRW

    for(i <- layout.bands.indices) {
      val oldBand = layout.bands(i)
      val newBand = newLayout.bands(i)
      val start = oldBand.previousHappening
      val end = oldBand.nextHappening
      val oldStates =
        (memoryLayout.stateOfHappening(start) to memoryLayout.stateOfHappening(end)).toSeq
      val newStates =
        (newLayout.stateOfHappening(start) to newLayout.stateOfHappening(end)).toSeq
      val oldTimes = oldStates.drop(1).foldLeft(Seq(0.0)) { case (l, s) => l :+ (l.last + s.dt) }
      val numOldDts = oldStates.size - 1
      val totalTime = oldTimes.last
      val newDt = totalTime / (newStates.size - 1)
      val newTimes = newStates.toSeq.indices.map(_ * newDt)

      for(i <- 1 until newStates.size - 1) {
        val targetT = newTimes(i)
        val before =
          oldStates.indices
            .dropRight(1)
            .find(i => oldTimes(i) <= targetT && targetT <= oldTimes(i + 1))
            .get
        assert(oldTimes(before) <= targetT && targetT <= oldTimes(before + 1))
        val beforeValue = oldStates(before).valueOfField(0)
        val afterValue = oldStates(before + 1).valueOfField(0)
        val originalDelta = oldTimes(before + 1) - oldTimes(before)
        val proportionBase = (targetT - oldTimes(before)) / originalDelta
        val proportion =
          if(proportionBase >= 0 && proportionBase <= 1)
            proportionBase
          else if(proportionBase < 0 && proportionBase > -1e-07)
            0.0
          else if(proportionBase > 1 && proportionBase < 1.0 + 1e-07)
            1.0
          else
            ???

        assert(proportion >= 0 && proportion <= 1)
        val newValue = beforeValue + (afterValue - beforeValue) * proportion

        val addr0 = newLayout.addressOfState(newStates(i))
        targetMem.write(addr0, newValue)
      }
      for(s <- newStates.tail) { // do not update DT of first happening
        val addrDt = newLayout.addressOfTime(s)
        if(!targetMem.isReadOnly(addrDt))
          targetMem.write(addrDt, newDt) //TODO: we ignore some DTs
      }
    }
    res
  }

  def doubleIgnoreTime(newLayout: MemoryLayout): Mem = {
    implicit val memory: RMemory = ioMem
    val res = Mem.makeFromLayout(newLayout)
    val targetMem: WMemory = res.memoryRW

    for(h <- layout.happenings) {
      val os = memoryLayout.stateOfHappening(h)
      val ns = newLayout.stateOfHappening(h)
      val value = os.valueOfField(0)
      targetMem.write(newLayout.addressOfState(ns, 0), value)
    }

    for(i <- layout.bands.indices) {
      val oldBand = layout.bands(i)
      val newBand = newLayout.bands(i)
      val start = oldBand.previousHappening
      val end = oldBand.nextHappening
      val sStart = memoryLayout.stateOfHappening(start)
      val sEnd = memoryLayout.stateOfHappening(end)
      val newStates =
        (newLayout.stateOfHappening(start) to newLayout.stateOfHappening(end)).toSeq

//      val oldTimes = oldStates.drop(1).foldLeft(Seq(0.0)) { case (l, s) => l :+ (l.last + s.dt) }
//      val numOldDts = oldStates.size - 1
      val startValue = sStart.valueOfField(0)
      val totalchange = sEnd.valueOfField(0) - startValue
      val dv = totalchange / (newStates.size - 1)
      val newValues = newStates.toSeq.indices.map(_ * dv + startValue)

      for(i <- 1 until newStates.size - 1) {

        val v = newValues(i)

        val addr0 = newLayout.addressOfState(newStates(i))
        targetMem.write(addr0, v)
      }
//      for(s <- newStates.tail) { // do not update DT of first happening
//        val addrDt = newLayout.addressOfTime(s)
//        targetMem.write(addrDt, newDt)
//      }
    }
    res
  }
}

object Mem {

  def makeFromLayout(layout: MemoryLayout): Mem = {
    val m = new MemImpl(baseSize = layout.lastAddress + 1)
    val statesWithReadOnlyTime =
      layout.bands.flatMap { bm =>
        Seq(bm.firstState.previous, bm.firstState)
      } :+ layout.bands.last.lastState.next

    statesWithReadOnlyTime.foreach(s => m.setReadOnly(layout.addressOfTime(s)))

    new Mem(m, layout)
  }

}
