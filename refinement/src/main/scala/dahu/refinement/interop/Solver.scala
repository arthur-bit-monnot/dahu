package dahu.refinement.interop
import dahu.model.products.ProductTagAny
import dahu.refinement.{Fun, MemImpl, RMemory, RefExpr}
import dahu.refinement.common.{Addr, Params, R, Values}
import dahu.refinement.interop.evaluation.Read
import dahu.refinement.la.LeastSquares

import scala.collection.mutable.ArrayBuffer

object Printer {
  def fmt(d: Double): String = "%1.2e".format(d)
}

abstract class Params(adds: Addr*) {
  def paramAddresses: Array[Addr] = adds.toArray
}
case class State0Param(addr: Addr) extends Params(addr)
case class State1Params(cs1: Addr, cs2: Addr, dt: Addr) extends Params(cs1, cs2, dt)
case class State2Params(cs1: Addr, cs2: Addr, cs3: Addr, dt1: Addr, dt2: Addr)
    extends Params(cs1, cs2, cs3, dt1, dt2)

case class MemoryLayout(bands: IndexedSeq[BandMem], cstate: ProductTagAny) {
  def apply(b: Int) = bands(b)

  def print(mem: RMemory): Unit = {
    for((b, i) <- bands.zipWithIndex) {
      println(s" --- Band $i")
      for(s <- 0 until b.numStates) {
        println(b.printState(s, mem))
      }

    }
  }

  def dtOfState(addr: Addr): Addr = addr + cstate.fields.size

  def states2(band: Int): Seq[State2Params] = {
    // if not the first band add the antelast state of the previous band
    val firstStates =
      if(band == 0) Seq()
      else Seq(bands(band - 1).addressOfState(bands(band - 1).numStates - 2))

    val lastStates =
      if(band == bands.size - 1) Seq()
      else Seq(bands(band + 1).addressOfState(1))

    val states: Seq[Addr] = firstStates ++ bands(band).states0.map(_.addr) ++ lastStates
    for(i <- 0 until states.size - 2) yield {
      val x = bands(band)
      State2Params(
        states(i),
        states(i + 1),
        states(i + 2),
        dtOfState(states(i + 1)),
        dtOfState(states(i + 2))
      )
    }
  }

}

case class BandMem(firstAddr: Addr, numStates: Int, cstate: ProductTagAny) {
  val stateSize: Int = cstate.fields.size
  def addressOfState(state: Int, fieldOffset: Int = 0): Addr =
    firstAddr + state * (stateSize + 1) + fieldOffset
  def addressOfTime(state: Int): Addr = addressOfState(state) + stateSize

  def printState(state: Int, mem: RMemory): String = {
    cstate.fields
      .map(f =>
        s"${addressOfState(state)}:  ${f.name}: ${Printer.fmt(mem.read(addressOfState(state, f.position)))} \t")
      .mkString("") + s" \t\t dt: ${Printer.fmt(mem.read(addressOfTime(state)))}"
  }

  def lastUsedAddr: Addr = addressOfTime(numStates)

  def states0: Seq[State0Param] =
    (0 until numStates).map(i => State0Param(addressOfState(i)))

  def states1: Seq[State1Params] =
    (0 until numStates - 1).map(i =>
      State1Params(addressOfState(i), addressOfState(i + 1), addressOfTime(i + 1)))

}

case class Tagged[X](value: X, level: Int)

class Solver(pb: Problem) {

  val mem = new MemImpl(baseSize = 100)

  type OfBand[X] = Seq[X]
  type B = Int
  val bands = pb.bands.indices

  val stateSize = pb.cstate.fields.size
  val numStates = 10
  val numStatesPerBand: OfBand[Int] = pb.bands.map(_ => numStates)

  val memories = MemoryLayout(
    bands
      .foldLeft(List[BandMem]()) {
        case (l, band) =>
          val firstAddr = l.headOption
            .map(m => m.addressOfState(m.numStates - 1)) // use last state from previous band
            .getOrElse(0)
          BandMem(firstAddr, numStatesPerBand(band) + 1, pb.cstate) :: l
      }
      .reverse
      .toIndexedSeq,
    pb.cstate
  )

  def states0(b: B) = memories(b).states0
  def states1(b: B) = memories(b).states1

  def build(): Seq[Tagged[RefExpr]] = {

    def applicables[X](qualifier: Qualifier, xs: Seq[X]) = qualifier match {
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
    def binds(s0: Params, c: Constraint): Array[Addr] = {
      c.fun.read.map {
        case Read(param, offset) => s0.paramAddresses(param) + offset
      }
    }
    val strictlyPositive = new Fun {
      override def numParams: B = 1
      override def eval(params: Values): R = {
        val dt = params(0)
        math.min(dt - 0.1, 0.0)
      }
    }

    for(band <- bands) {
      val bm = memories(band)
      for(s <- 0 until bm.numStates) {
        val dtAddr = bm.addressOfTime(s)
        record(strictlyPositive.bind(dtAddr), level = 0)
      }

      val simpleStates = states0(band)

      for(c <- pb.bands(band).constraints0) {
        val f = c.fun

        val states = applicables(c.qual, simpleStates)
        states.foreach { s0 =>
          val x = binds(s0, c)
          record(f.bindArray(x), level = 0)
        }

      }
      val dualStates = states1(band)
      for(c <- pb.bands(band).constraints1) {
        val f = c.fun

        val states = applicables(c.qual, dualStates)
        println(states)
        states.foreach { s0 =>
          val x = binds(s0, c)
          record(f.bindArray(x), level = 1)
        }

      }
      val tripleStates = memories.states2(band)
      for(c <- pb.bands(band).constraints2) {
        val f = c.fun

        val states = applicables(c.qual, tripleStates)
        println(states)
        states.foreach { s0 =>
          val x = binds(s0, c)
          record(f.bindArray(x), level = 2)
        }

      }

    }
    exprs
  }

  def solve(numIters: Int = 100000): Unit = {
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
