package dahu.refinement.interop
import java.io.{BufferedWriter, File, FileWriter}

import dahu.model.products.{Field, ProductTagAny}
import dahu.refinement._
import dahu.refinement.common.{Addr, Params, R, Values}
import dahu.refinement.interop.Mode.{FinalSat, InitSat, Optim}
import dahu.refinement.memory._
import dahu.refinement.interop.evaluation.Read
import dahu.refinement.la.{LeastSquares, SolverStats, Stats}
import dahu.utils.errors._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Printer {
  def fmtFloat(d: Double): String = "%1.2f".format(d)
  def fmt(d: Double): String = "%1.2e".format(d)
}

case class Params(
    statesPerBand: Int = 2,
    innerLoops: Int = 30,
    targetDt: Double = 0.1,
    dtErrorRatio: Double = 1.1,
    maxLevels: Int = 2,
    maxScalings: Int = 15,
    maxStatePerBand: Int = 500,
    maxHomogenization: Int = 10,
    targetInitError: Double = 1e-2,
    targetFinalError: Double = 1e-3
)

sealed trait Mode
object Mode {
  case object InitSat extends Mode
  case object Optim extends Mode
  case object FinalSat extends Mode
}

sealed trait SolverLevel
case class Searching(
    level: Int,
    mode: Mode,
    scalingLeft: Int,
    homogenizationLeft: Int
) extends SolverLevel
case object Succeeded extends SolverLevel
case object Failed extends SolverLevel

case class Tagged[X](value: X, level: Int, mode: Option[Searching => Boolean])

final class Solver(pb: Problem, params: Params) {

  implicit private def implicitParams: Params = params

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

  def build(layout: MemoryLayout, currentLevel: Int): Seq[Tagged[RefExpr]] =
    bands.flatMap(i => build(layout, i, params.targetDt, currentLevel))

  def build(layout: MemoryLayout,
            bandId: Int,
            targetDt: Double,
            currentLvl: Int): Seq[Tagged[RefExpr]] = {
    val exprs = ArrayBuffer[Tagged[RefExpr]]()
    def record(_e: RefExpr, level: Int, mode: Option[Searching => Boolean], scale: Double): Unit = {
      val e =
        if(scale == 1)
          _e
        else
          _e.scale(scale)
//      if(e.params.length == 0) {
//        // constant
//        assert(e.eval(new MemImpl()) == 0.0, "Constant constraint that is non-zero")
//      } else {
      exprs += Tagged(e, level, mode)
//      }
    }
    def binds(s: State, c: Constraint): (Array[Addr], Array[Double]) = {
      val addr = c.fun.read.map {
        case Read(relState, offset) => {
          layout.addressOfState(s + relState, offset)
        }
      }
      val gradientFactors = c.fun.read.map {
        case Read(relState, offset) => {
          // gradient fact by rel state:
          // 0 => 0
          // 1 => 1.1
          // -1 => 0.9
          1.0 //- 0.1 * relState
        }
      }
      (addr, gradientFactors)
    }
    val strictlyPositive = new Fun {
      override def numParams: B = 1
      override def eval(params: Values): R = {
        val dt = params(0)
        (dt - targetDt) * 1
//        math.min(dt - targetDt, 0.0) * 10000
      }
    }
    val conservater = new Fun {
      override def numParams: B = 2
      override def eval(params: Values): R = {
        params(1) - params(0)
      }
    }

    def conservation(s: State, field: Int): Option[RefExpr] = {
      if(s <= layout.firstState)
        None
      else if(layout.lastState <= s)
        None
      else {
        val params = Array(
          layout.addressOfState(s.previous, field),
          layout.addressOfState(s, field),
        )
        val gradFactors = Array(0.0, 1.0)
        Some(conservater.bindArray(params, gradFactors))
      }
    }
    val dtBelowMax = new Fun {
      override def numParams: B = 1
      override def eval(params: Values): R = {
        val dt = params(0)
        math.min(targetDt * implicitParams.dtErrorRatio - dt, 0.0) * 100
      }
    }

//    val minimizeStrong = new MinimizationFun(0.10, 0.001)
//    val minimizeLow = new MinimizationFun(0.1, 0.0001)

    val minimizeStrong = new Fun {
      override def numParams: B = 1
      override def eval(params: Values): R = {
        val dt = params(0)
        (dt - targetDt) * 0.0001
      }
    }
    val bm = layout(bandId)
    val allStates = bm.firstState.previous to bm.lastState.next

    for(s <- allStates) {
      for(field <- layout.nonDtFields)
        conservation(s, field).foreach(
          e => {
            record(e, level = 0, Some(l => l.level == 0 && l.mode == InitSat), 1)
//          record(e, level = 0, Some(l => true), 1)

          }
        )

      record(strictlyPositive.bindSimple(layout.addressOfTime(s)),
             level = 0,
             Some(_.mode == InitSat),
             0.1)
      record(strictlyPositive.bindSimple(layout.addressOfTime(s)),
             level = 0,
             Some(_.mode == FinalSat),
             0.001)
//      record(minimizeStrong.bind(layout.addressOfTime(s)), level = 0, Some(InitSat), 1)
//      record(dtBelowMax.bind(layout.addressOfTime(s)), level = 0, Some(FinalSat), 1)
//      record(minimizeStrong.bind(layout.addressOfTime(s)), level = 1, Some(Optim))
//      record(minimizeLow.bind(layout.addressOfTime(s)), level = 1, Some(FinalSat))
    }

    def applyOn(states: Iterable[State], c: Constraint): Unit = {
      val f = c.fun
      states
        .filter(
          s =>
            layout.firstState <= s + f.belowStateOffset &&
              (s + f.aboveStateOffset) <= layout.lastState)
        .foreach { s0 =>
          val (addr, factors) = binds(s0, c)

          val lvl = f.aboveStateOffset - f.belowStateOffset
          val scale = math.pow(10, currentLvl - lvl)

          record(f.bindArray(addr, factors), level = lvl, None, scale)
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
        else if(c.fun.belowStateOffset == 0 && c.fun.aboveStateOffset == 0)
          s.previous
        else
          s
      applyOn(as :: Nil, c)
    }
    exprs
  }

  def solve(): Either[String, Mem] = {
    val mem = Mem.makeFromLayout(layout)
    solve(mem)
  }

  def next(l: Searching, mem: Mem, stats: SolverStats): (SolverLevel, Mem) = {
    val target = params.targetDt * 0.9 //* params.dtErrorRatio * 0.9
    def makeFinal(lvl: Int) = Searching(lvl, FinalSat, 0, params.maxHomogenization)
    def makeInit(lvl: Int) = Searching(lvl, InitSat, params.maxScalings, params.maxHomogenization)
    def shouldScale: Boolean =
      l.scalingLeft > 0 && mem
        .grownLayout(target)
        .nonEmpty

    l match {
      case Searching(n, FinalSat, 0, 0) if stats.maxResidual > params.targetFinalError =>
        println("Failed")
        mem.printCSV("/tmp/traj.csv")
        println(s"succ: ${Stats.numSuccess} -- failed: ${Stats.numFailed}")
        (Failed, mem)
      case Searching(n, FinalSat, _, _) if stats.maxResidual <= params.targetFinalError =>
        println("shortcut")
        if(n == params.maxLevels)
          (Succeeded, mem)
        else
          (makeInit(n + 1), mem)
      case Searching(n, FinalSat, 0, 0) =>
        assert(n < params.maxLevels)
        mem.homogenize()
        (makeInit(n + 1), mem)

      case Searching(n, FinalSat, 0, h) =>
        mem.homogenize()
        (Searching(n, FinalSat, 0, h - 1), mem)

      case Searching(n, InitSat, 0, 0) =>
        mem.homogenize()
        mem.printCSV("/tmp/traj-init.csv")
        (makeFinal(n), mem)

      case Searching(n, InitSat, _, _)
          if stats.maxResidual < params.targetInitError && !shouldScale => // go to next level directly
        println("Shortcut")
        mem.printCSV("/tmp/traj-init.csv")

        mem.homogenize()
        (makeFinal(n), mem)

      case Searching(n, InitSat, s, homogenizationLeft) if homogenizationLeft > 0 =>
        mem.homogenize()
        (Searching(n, InitSat, s, homogenizationLeft - 1), mem)

      case Searching(n, InitSat, scalingLeft, homogenizationLeft) if shouldScale =>
        assert(scalingLeft > 0)
        mem
          .grownLayout(target) match {
          case Some(newLayout) =>
            val mem2 = mem.doubleTimeBased(newLayout)
            (Searching(n, InitSat, scalingLeft - 1, params.maxHomogenization), mem2)
          case None => unexpected
        }

      case Searching(n, InitSat, scalingLeft, homogenizationLeft) =>
        assert(!shouldScale)
        if(homogenizationLeft > 0) {
          mem.homogenize()
          (Searching(n, InitSat, scalingLeft, homogenizationLeft - 1), mem)
        } else {
          mem.printCSV("/tmp/traj-init.csv")
          (makeFinal(n), mem)
        }
    }
  }

  def solve(mem: Mem, l: Searching, maxIters: Int): (Mem, SolverStats) = {
    print(s"lvl: $l -- #states: ${mem.layout.allStates.size}... ")

    val levelFilter: Tagged[RefExpr] => Boolean = l match {
      case Searching(n, mode, _, _) =>
        c =>
          c.level <= n && c.mode.forall(f => f(l))
    }
    val es = build(mem.layout, l.level)
    val constraints = es
      .filter(levelFilter)
      .map(_.value)

    val ls = new LeastSquares(constraints)

    val lm = ls.lmIterator(mem.memoryRW)

    for(_ <- 0 until maxIters) {
      if(lm.shouldContinue) {
//        mem.homogenize()
        lm.next()
      }
    }
    val stats = lm.stats
    println("Max residual: " + Printer.fmt(stats.maxResidual))
    mem.print()
    (mem, lm.stats)
  }

  def solve(mem: Mem): Either[String, Mem] = {

    var currentLevel: SolverLevel =
      Searching(0, InitSat, params.maxScalings, params.maxHomogenization)
    var currentMem = mem
    val history = ArrayBuffer[(SolverLevel, SolverStats)]()

    while(true) {
      currentLevel match {
        case x: Searching =>
          val res = solve(currentMem, x, params.innerLoops)
          res match {
            case (newMem, stats) =>
              currentMem = newMem
              next(x, currentMem, stats) match {
                case (l, m) =>
                  currentLevel = l
                  currentMem = m
              }
          }
        case Succeeded =>
          currentMem.printCSV("/tmp/traj.csv")
          return Right(currentMem)
        case Failed =>
          return Left("Failure")
      }

    }
    unexpected
  }
}
