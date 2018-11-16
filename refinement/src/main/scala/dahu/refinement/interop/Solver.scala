package dahu.refinement.interop
import java.io.{BufferedWriter, File, FileWriter}

import dahu.model.products.{Field, ProductTagAny}
import dahu.refinement._
import dahu.refinement.common.{Addr, Params, R, Values}
import dahu.refinement.interop.Mode.{FinalSat, InitSat, Optim}
import dahu.refinement.memory._
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
    statesPerBand: Int = 0,
    innerLoops: Int = 100,
    targetDt: Double = 0.1,
    maxLevels: Int = 2
)

sealed trait Mode
object Mode {
  case object InitSat extends Mode
  case object Optim extends Mode
  case object FinalSat extends Mode
}

case class SolverLevel(
    maxInvolvedStates: Int,
    mode: Mode
)

case class Tagged[X](value: X, level: Int, mode: Option[Mode])

final class Solver(pb: Problem, params: Params) {

  type OfBand[X] = Seq[X]
  type B = Int
  val bands = pb.bands.indices

  val stateSize = pb.cstate.fields.size
  val numStates = params.statesPerBand
  val numStatesPerBand: OfBand[Int] = pb.bands.map(_ => numStates)

  private val layout = MemoryLayout(
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

  def build(layout: MemoryLayout): Seq[Tagged[RefExpr]] =
    bands.flatMap(i => build(layout, i, params.targetDt))

  def build(layout: MemoryLayout, bandId: Int, targetDt: Double): Seq[Tagged[RefExpr]] = {
    val exprs = ArrayBuffer[Tagged[RefExpr]]()
    def record(e: RefExpr, level: Int, mode: Option[Mode]): Unit = {
      if(e.params.length == 0) {
        // constant
        assert(e.eval(new MemImpl()) == 0.0, "Constant constraint that is non-zero")
      } else {
        exprs += Tagged(e, level, mode)
      }
    }
    def binds(s: State, c: Constraint): Array[Addr] = {
      c.fun.read.map {
        case Read(relState, offset) =>
          layout.addressOfState(s + relState, offset)
      }
    }
    val strictlyPositive = new Fun {
      override def numParams: B = 1
      override def eval(params: Values): R = {
        val dt = params(0)
        math.min(dt - targetDt, 0.0)
      }
    }

    val minimizeStrong = new MinimizationFun(0.1, 0.001)
    val minimizeLow = new MinimizationFun(0.1, 0.0001)

//    val minimizeStrong = new Fun {
//      override def numParams: B = 1
//      override def eval(params: Values): R = {
//        val dt = params(0)
//        math.sqrt(math.abs(dt - targetDt) / 10)
//        (dt - (targetDt)) * 0.01
//      }
//    }
//    val minimizeLow = new Fun {
//      override def numParams: B = 1
//      override def eval(params: Values): R = {
//        val dt = params(0)
//        (dt - (targetDt)) * 0.0001
//      }
//    }
    val bm = layout(bandId)
    val allStates = bm.firstState.previous to bm.lastState.next

    for(s <- allStates) {
      record(strictlyPositive.bind(layout.addressOfTime(s)), level = 0, None)
      record(minimizeStrong.bind(layout.addressOfTime(s)), level = 1, Some(Optim))
      record(minimizeLow.bind(layout.addressOfTime(s)), level = 1, Some(FinalSat))
    }

    def applyOn(states: Iterable[State], c: Constraint): Unit = {
      val f = c.fun
      states
        .filter(
          s =>
            layout.firstState <= s + f.belowStateOffset &&
              (s + f.aboveStateOffset) <= layout.lastState)
        .foreach { s0 =>
          val x = binds(s0, c)

//            println(x.toSeq.toString() + "   " + f.read.toSeq)

          record(f.bindArray(x), level = f.aboveStateOffset - f.belowStateOffset, None)
        }
    }

//      println("start")
    for(c <- pb.bands(bandId).constraintsStart) {
      val s = layout.stateOfHappening(bm.previousHappening)
      val as =
        if(c.fun.belowStateOffset == -1 && c.fun.aboveStateOffset == 0)
          s.next // speed constraint, involving the previous state..., apply on next
        else
          s
      applyOn(as :: Nil, c)
    }
//      println("middle")
    for(c <- pb.bands(bandId).constraintsMiddle) {
      applyOn(bm.states, c)
    }
//      println("end")
    for(c <- pb.bands(bandId).constraintsEnd) {
      val s = layout.stateOfHappening(bm.nextHappening)
      val as =
        if(c.fun.belowStateOffset == 0 && c.fun.aboveStateOffset == 1)
          s.previous // speed constraint, involving the next state..., apply on previous
        else
          s
      applyOn(as :: Nil, c)
    }
    exprs
  }

  def solve(): Mem = {
    val mem = Mem.makeFromLayout(layout)
    solve(mem)
    mem
  }

  def next(l: SolverLevel, mem: Mem): Option[(SolverLevel, Mem)] = l match {
    case SolverLevel(n, InitSat) =>
      Some((SolverLevel(n, Optim), mem))
    case SolverLevel(n, Optim) =>
      return Some((SolverLevel(n, FinalSat), mem))
      val dt = params.targetDt * 10
      if(mem.layout.bands.exists(mem.averageDt(_) > dt)) {
        Some((SolverLevel(n, Optim), mem.double(dt)))
      } else {
        Some((SolverLevel(n, FinalSat), mem))
      }

    case SolverLevel(n, FinalSat) if n >= params.maxLevels => None
    case SolverLevel(n, FinalSat) =>
      Some((SolverLevel(n + 1, InitSat), mem))
  }

  def solve(mem: Mem, l: SolverLevel, maxIters: Int): Either[String, Mem] = {
    val levelFilter: Tagged[RefExpr] => Boolean = l match {
      case SolverLevel(n, mode) =>
        c =>
          c.level <= n && c.mode.forall(_ == mode)
    }
    val es = build(mem.layout)
    val constraints = es
      .filter(levelFilter)
      .map(_.value)

    val ls = new LeastSquares(constraints)

    val lm = ls.lmIterator(mem.memoryRW)

    for(i <- 0 until maxIters) {
      if(lm.shouldContinue) {
        mem.homogenize()
        lm.next()
      }
    }
    println("Error: " + lm.lastChi)
    Right(mem)
  }

  def solve(mem: Mem): Either[String, Mem] = {
    val initialLevel = SolverLevel(0, InitSat)

    var currentLevel = Option(initialLevel)
    var currentMem = mem

    while(true) {
      println(currentLevel)

      currentLevel match {
        case Some(x) =>
          val res = solve(currentMem, x, params.innerLoops)
          res match {
            case Right(newMem) =>
              currentMem = newMem
            case Left(err) =>
              return Left(err)
          }
          currentMem.print()
          next(x, currentMem) match {
            case Some((l, m)) =>
              currentLevel = Some(l)
              currentMem = m
            case None =>
              currentLevel = None
          }
        case None =>
          currentMem.print()
          currentMem.printCSV("/tmp/traj.csv")
          return Right(currentMem)
      }

    }
    ???
  }

//  def solve(mem: Mem): Unit = {
//    val es = build(mem.layout)
//
//    for(lvl <- 0 to params.maxLevels) {
//      val constraints = es.filter(_.level <= lvl).map(_.value)
//
//      val ls = new LeastSquares(constraints)
//
//      val lm = ls.lmIterator(mem.memoryRW)
//
//      for(i <- 0 until params.innerLoops) {
////        println(s"\n\n\n -------------- LEVEL $lvl % $i-------------")
////        println("  before homog")
////        memories.print(mem)
//        if(lm.shouldContinue) {
//          mem.homogenize()
//          lm.next()
//        }
////        memories.bands.foreach(b => homogenizeInPlace(b, mem)) //TODO
////        println("  after homog")
////        memories.print(mem)
////        ls.lmIteration(mem.memoryRW, 1)
//
////        println("  after optim")
////        memories.print(mem)
//
//      }
//
//      println(s" -------------- LEVEL $lvl -------------")
//      mem.print()
//      mem.homogenize()
//      mem.printCSV("/tmp/traj.csv")
//      mem.print()
//
//    }
//
////    memories.print(mem)
//
//  }
}
