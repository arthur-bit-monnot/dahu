package dahu.refinement.interop
import dahu.model.products.ProductTagAny
import dahu.refinement.{Fun, MemImpl, RMemory, RefExpr}
import dahu.refinement.common.{Addr, Params, R, Values}
import dahu.refinement.interop.MemTypes.State
import dahu.refinement.interop.evaluation.Read
import dahu.refinement.la.LeastSquares
import dahu.utils.errors._

import scala.collection.mutable.ArrayBuffer

object Printer {
  def fmt(d: Double): String = "%1.2e".format(d)
}

object MemTypes {

  class State(val i: Int) extends AnyVal {
    def +(off: Int): State = new State(i + off)

    def <(s: State): Boolean = i < s.i
    def <=(s: State): Boolean = i <= s.i

    def to(lastState: State): Iterable[State] =
      (i to lastState.i).map(x => new State(x))
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

  def printState(state: State, mem: RMemory): String = {
    s"${addressOfState(state)}: " +
      cstate.fields
        .map(f => s"  ${f.name}: ${Printer.fmt(mem.read(addressOfState(state, f.position)))} \t")
        .mkString("")
  }

}

case class BandMem(firstState: State, numStates: Int, cstate: ProductTagAny) {
  def lastState: State = firstState + (numStates - 1)
  def states = firstState to lastState

}

case class Tagged[X](value: X, level: Int)

class Solver(pb: Problem) {

  val mem = new MemImpl(baseSize = 100)

  type OfBand[X] = Seq[X]
  type B = Int
  val bands = pb.bands.indices

  val stateSize = pb.cstate.fields.size
  val numStates = 2
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

//    for(s <- memories.allStates) {
//      record(strictlyPositive.bind(memories.addressOfTime(s)), level = 0)
//    }

    for(band <- bands) {

      val bm = memories(band)
      val bandStates = bm.firstState to bm.lastState

      for(c <- pb.bands(band).constraints0) {
        val f = c.fun

        println(f.read.toSeq)
        val isFirstDeriv = f.belowStateOffset == -1 && f.aboveStateOffset == 0
        val filtered =
          if(isFirstDeriv) bandStates.tail
          else bandStates
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

  def solve(numIters: Int = 10): Unit = {
    val es = build()

    for(lvl <- 0 to 2) {
      val constraints = es.filter(_.level <= lvl).map(_.value)

      val ls = new LeastSquares(constraints)

      ls.lmIteration(mem, numIters)

      println(s" -------------- LEVEL $lvl -------------")
      memories.print(mem)
    }

  }
}
