package dahu.refinement.interop
import java.io.{BufferedWriter, File, FileWriter}

import dahu.model.products.{Field, ProductTagAny}
import dahu.refinement._
import dahu.refinement.common.{Addr, Params, R, Values}
import dahu.refinement.interop.MemTypes.State
import dahu.refinement.interop.evaluation.Read
import dahu.refinement.la.LeastSquares
import dahu.utils.errors._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Printer {
  def fmt(d: Double): String = "%1.2f".format(d)
}

case class Params(
    statesPerBand: Int = 150,
    outerLoops: Int = 10,
    innerLoops: Int = 40
)

object MemTypes {

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

}
import MemTypes._

case class MemoryLayout(bands: IndexedSeq[BandMem], cstate: ProductTagAny) {

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

  def apply(b: Int) = bands(b)

  def print(mem: RMemory): Unit = {
    for((b, i) <- bands.zipWithIndex) {
      val h = new Happening(i)
      println(s"--- HAPPENING ${i}\n${printState(stateOfHappening(h), mem)}")
      println(s" --- Band $i")
      for(s <- b.states) {
        println(printState(s, mem))
      }
      if(i == bands.size - 1)
        println(s"--- HAPPENING ${i + 1}\n${printState(stateOfHappening(h.next), mem)}")

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

case class BandMem(previousHappening: Happening,
                   firstState: State,
                   numStates: Int,
                   cstate: ProductTagAny) {
  def nextHappening: Happening = previousHappening.next
  def lastState: State = firstState + (numStates - 1)
  def states = firstState to lastState
  def state(i: Int) = firstState + i

}

case class Tagged[X](value: X, level: Int)

class Solver(pb: Problem, params: Params) {

  val mem = new MemImpl(baseSize = 1000)

  type OfBand[X] = Seq[X]
  type B = Int
  val bands = pb.bands.indices

  val stateSize = pb.cstate.fields.size
  val numStates = params.statesPerBand
  val numStatesPerBand: OfBand[Int] = pb.bands.map(_ => numStates)

  val memories = MemoryLayout(
    bands
      .foldLeft(List[BandMem]()) {
        case (l, band) =>
          val firstState = l.headOption
            .map(m => m.lastState.next.next) // use last state from previous band
            .getOrElse(new State(0).next)
          BandMem(new Happening(band), firstState, numStatesPerBand(band), pb.cstate) :: l
      }
      .reverse
      .toIndexedSeq,
    pb.cstate
  )
  def homogenizeInPlace(b: BandMem, ioMem: RWMemory): Unit =
    homogenizeInPlace(b.previousHappening, b.nextHappening, ioMem)
  def homogenizeInPlace(start: Happening, end: Happening, ioMem: RWMemory): Unit = {
    implicit val memoryLayout = memories
    implicit val memory: RMemory = ioMem.copy

    val states = (memoryLayout.stateOfHappening(start) to memoryLayout.stateOfHappening(end)).toSeq
    val times = states.drop(1).foldLeft(Seq(0.0)) { case (l, s) => l :+ (l.last + s.dt) }
    val numDts = states.size - 1
    val totalTime = times.last
    val dt = totalTime / numDts
    val newTimes = states.toSeq.indices.map(_ * dt)

    println("A")
    for(i <- 1 until states.size - 1) {
      val targetT = newTimes(i)
      val before =
        states.indices.dropRight(1).find(i => times(i) <= targetT && targetT <= times(i + 1)).get
      assert(times(before) <= targetT && targetT <= times(before + 1))
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

      val addr0 = memoryLayout.addressOfState(states(i))
      ioMem.write(addr0, newValue)
    }
    for(s <- states.tail) { // do not update DT of first happening
      val addrDt = memoryLayout.addressOfTime(s)
      ioMem.write(addrDt, dt)
    }

  }

//  def homogenizeInPlace(band: BandMem, ioMem: RWMemory, tighteningFactor: Double): Unit = {
//    implicit val memoryLayout = memories
//    implicit val memory: RMemory = ioMem.copy
//
//    val states = band.firstState.previous to band.lastState.next
//
//    val totalTime = states.iterator.map(_.dt).sum
//    val numStates = states.size - 1
//
//    @tailrec def valueAt(field: Int, targetTime: Double, s: State, currentTime: Double): Double = {
//      assert(targetTime >= currentTime)
//      assert(s.next.dt > 0)
//      if(targetTime <= currentTime + s.next.dt) {
//
//        val proportion = (targetTime - currentTime) / s.next.dt
//
//        assert(proportion >= 0 && proportion <= 1.0001)
//        val v1 = s.valueOfField(field)
//        val v2 = s.next.valueOfField(field)
//        val X = v1 + (v2 - v1) * proportion
////        assert(X >= 0) // sanity check
//        X
//      } else {
//        valueAt(field, targetTime, s.next, currentTime + s.next.dt)
//      }
//    }
//
//    val dt = totalTime / numStates
//    var s = band.firstState
//    var i = 1
//    while(s <= band.lastState.next) {
//      val t = dt * i
//      val addr0 = memories.addressOfState(s)
//      val value = valueAt(0, t, band.firstState.previous, 0)
//      println(s"$addr0 $t $value $dt")
//      ioMem.write(addr0, value)
//      ioMem.write(addr0 + 1, dt * tighteningFactor)
//      s = s.next
//      i = i + 1
//    }
//    assert(s == band.lastState.next.next)
//  }

  def build(): Seq[Tagged[RefExpr]] = {

    val exprs = ArrayBuffer[Tagged[RefExpr]]()
    def record(e: RefExpr, level: Int): Unit = {
      if(e.params.length == 0) {
        // constant
        assert(e.eval(new MemImpl()) == 0.0, "Constant constraint that is non-zero")
      } else {
        exprs += Tagged(e, level)
      }
    }
    def binds(s: State, c: Constraint): Array[Addr] = {
      c.fun.read.map {
        case Read(relState, offset) =>
          memories.addressOfState(s + relState, offset)
      }
    }
    val strictlyPositive = new Fun {
      override def numParams: B = 1
      override def eval(params: Values): R = {
        val dt = params(0)
        math.min(dt - 0.1, 0.0)
      }
    }
    val minimize = new Fun {
      override def numParams: B = 1
      override def eval(params: Values): R = {
        val dt = params(0)
        math.sqrt((dt - 0.01) * 1000) * 0.01
        (dt - 0.1) * 0.1
      }
    }

    for(s <- memories.allStates) {
      record(strictlyPositive.bind(memories.addressOfTime(s)), level = 0)
      record(minimize.bind(memories.addressOfTime(s)), level = 3)
    }

    for(band <- bands) {
      println(s"Band: $band")

      val bm = memories(band)

      def applyOn(states: Iterable[State], c: Constraint): Unit = {
        val f = c.fun
        states
          .filter(
            s =>
              memories.firstState <= s + f.belowStateOffset &&
                (s + f.aboveStateOffset) <= memories.lastState)
          .foreach { s0 =>
            val x = binds(s0, c)

            println(x.toSeq.toString() + "   " + f.read.toSeq)

            record(f.bindArray(x), level = f.aboveStateOffset - f.belowStateOffset)
          }
      }

      println("start")
      for(c <- pb.bands(band).constraintsStart) {
        val s = memories.stateOfHappening(bm.previousHappening)
        val as =
          if(c.fun.belowStateOffset == -1 && c.fun.aboveStateOffset == 0)
            s.next // speed constraint, involving the previous state..., apply on next
          else
            s
        applyOn(as :: Nil, c)
      }
      println("middle")
      for(c <- pb.bands(band).constraintsMiddle) {
        applyOn(bm.states, c)
      }
      println("end")
      for(c <- pb.bands(band).constraintsEnd) {
        val s = memories.stateOfHappening(bm.nextHappening)
        val as =
          if(c.fun.belowStateOffset == 0 && c.fun.aboveStateOffset == 1)
            s.previous // speed constraint, involving the next state..., apply on previous
          else
            s
        applyOn(as :: Nil, c)
      }

    }
//    sys.exit(20)
    exprs
  }

  def solve(): Unit = {
    val es = build()
//    sys.exit(30)

    for(lvl <- 0 to 3) {
      val constraints = es.filter(_.level <= lvl).map(_.value)

      val ls = new LeastSquares(constraints)

      for(i <- 0 until params.outerLoops) {
//        println(s"\n\n\n -------------- LEVEL $lvl % $i-------------")
//        println("  before homog")
//        memories.print(mem)
        memories.bands.foreach(b => homogenizeInPlace(b, mem)) //TODO
//        println("  after homog")
//        memories.print(mem)
        ls.lmIteration(mem, params.innerLoops)

//        println("  after optim")
//        memories.print(mem)

      }

      println(s" -------------- LEVEL $lvl -------------")
      memories.print(mem)
      memories.bands.foreach(b => homogenizeInPlace(b, mem)) //TODO
      memories.printCSV(mem, "/tmp/traj.csv")
      memories.printCSV(mem, s"/tmp/traj-$lvl.csv")
      memories.print(mem)

    }

//    memories.print(mem)

  }
}
