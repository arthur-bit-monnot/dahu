package dahu.refinement.interop
import dahu.model.products.{Field, ProductTagAny}
import dahu.refinement._
import dahu.refinement.common.{Addr, Params, R, Values}
import dahu.refinement.interop.MemTypes.State
import dahu.refinement.interop.evaluation.Read
import dahu.refinement.la.LeastSquares
import dahu.utils.errors._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Printer {
  def fmt(d: Double): String = "%1.2f".format(d)
}

case class Params(
    statesPerBand: Int = 30,
    outerLoops: Int = 5,
    innerLoops: Int = 20
)

object MemTypes {

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
  def lastState: State = bands.last.firstState + bands.last.numStates
  (firstState.i to lastState.i).map(i => new State(i))

  def allStates = firstState to lastState

  private val dtOffset = cstate.fieldPosition("dt").getOrElse(unexpected("cstate has no dt field"))

  val firstStateAddress = 0
  val stateSize: Int = cstate.fields.size
  def addressOfState(s: State, fieldNum: Int = 0): Addr =
    firstStateAddress + (stateSize * s.i) + fieldNum

  def addressOfTime(s: State): Int = addressOfState(s, dtOffset)

  def apply(b: Int) = bands(b)

  def print(mem: RMemory): Unit = {
    for((b, i) <- bands.zipWithIndex) {
      println(s" --- Band $i")
      for(s <- b.states) {
        println(printState(s, mem))
      }

    }
  }

//  def printState(state: State, mem: RMemory): String = {
//    s"${addressOfState(state)}: " +
//      cstate.fields
//        .map(f => s"  ${f.name}: ${Printer.fmt(mem.read(addressOfState(state, f.position)))} \t")
//        .mkString("")
//  }
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

case class BandMem(firstState: State, numStates: Int, cstate: ProductTagAny) {
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
            .map(m => m.lastState) // use last state from previous band
            .getOrElse(new State(0))
          BandMem(firstState, numStatesPerBand(band) + 1, pb.cstate) :: l
      }
      .reverse
      .toIndexedSeq,
    pb.cstate
  )

  def homogenizeInPlace(band: BandMem, ioMem: RWMemory, tighteningFactor: Double): Unit = {
    implicit val memoryLayout = memories
    implicit val memory: RMemory = ioMem.copy

    val totalTime = (band.firstState.next to band.lastState).map(_.dt).sum
    val numStates = band.numStates - 1

    @tailrec def valueAt(field: Int, targetTime: Double, s: State, currentTime: Double): Double = {
      assert(targetTime >= currentTime)
      assert(s.next.dt > 0)
      if(targetTime <= currentTime + s.next.dt) {

        val proportion = (targetTime - currentTime) / s.next.dt

        assert(proportion >= 0 && proportion <= 1.0001)
        val v1 = s.valueOfField(field)
        val v2 = s.next.valueOfField(field)
        val X = v1 + (v2 - v1) * proportion
        assert(X >= 0)
        X
      } else {
        valueAt(field, targetTime, s.next, currentTime + s.next.dt)
      }
    }

    val dt = totalTime / numStates
    for(i <- 1 until band.numStates) {
      val t = dt * i
      val s = band.state(i)
      val addr0 = memories.addressOfState(s)
      val value = valueAt(0, t, band.firstState, 0)
      ioMem.write(addr0, value)
      ioMem.write(addr0 + 1, dt * tighteningFactor)
    }
  }

  def build(): Seq[Tagged[RefExpr]] = {

    def applicables[X](qualifier: Qualifier, xs: Iterable[X]) = qualifier match {
      case Qualifier.Always  => xs
      case Qualifier.AtStart => Seq(xs.head)
      case Qualifier.AtEnd   => Seq(xs.last)
    }

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
        math.sqrt((dt - 0.1) * 1000) * 0.01
        (dt - 0.1) * 0.01
      }
    }

    for(s <- memories.allStates) {
      record(strictlyPositive.bind(memories.addressOfTime(s)), level = 0)
//      record(minimize.bind(memories.addressOfTime(s)), level = 1)
    }

    for(band <- bands) {

      val bm = memories(band)
      val bandStates = bm.firstState to bm.lastState

      for(c <- pb.bands(band).constraints0) {
        val f = c.fun

        println(f.read.toSeq)
//        val isFirstDeriv = f.belowStateOffset == -1 && f.aboveStateOffset == 0
        val filtered = bandStates
        val states =
          applicables(c.qual, filtered)
            .filter(
              s =>
                memories.firstState <= s + f.belowStateOffset &&
                  (s + f.aboveStateOffset) <= memories.lastState)

        states.foreach { s0 =>
          val x = binds(s0, c)
          record(f.bindArray(x), level = f.aboveStateOffset - f.belowStateOffset)
        }

      }

    }
    exprs
  }

  def solve(): Unit = {
    val es = build()

    for(lvl <- 0 to 2) {
      val constraints = es.filter(_.level <= lvl).map(_.value)

      val ls = new LeastSquares(constraints)

      for(i <- 0 until params.outerLoops) {
//        println(s"\n\n\n -------------- LEVEL $lvl % $i-------------")
//        println("  before homog")
//        memories.print(mem)
        memories.bands.foreach(b => homogenizeInPlace(b, mem, 1))
//        println("  after homog")
//        memories.print(mem)
        ls.lmIteration(mem, params.innerLoops)

//        println("  after optim")
//        memories.print(mem)

      }

      println(s" -------------- LEVEL $lvl -------------")
      memories.print(mem)

    }

//    memories.print(mem)

  }
}
