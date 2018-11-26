package dahu.refinement.la

import dahu.matrix.{Matrix, MatrixFactory}

import scala.util.Try

import dahu.refinement.common._
import dahu.refinement._

class LeastSquares(allResiduals: Seq[RefExpr]) {

  def activeResiduals(mem: RMemory): Seq[RefExpr] =
    allResiduals //.filter(_.eval(mem) >= 1e-4)

//  val numVars: Int = residuals.map(_.params.max).max

  def evalResiduals(memory: RMemory): Seq[R] = {
    activeResiduals(memory).map(_.eval(memory))
  }

  def jacobian(memory: RMemory): Matrix = {
    val jac = new MatrixFactory(activeResiduals(memory).size, 0)
//    println("\nGradients")
    for((e, i) <- activeResiduals(memory).zipWithIndex) {
      val gradj = e.gradient(memory)
//      println(gradj.mkString("\t"))
      var x = 0
      while(x < e.params.length) {
        val j = e.params(x)
//        if(j == 6) {
//          println("COnstraint on 6")
//          println("params: " + e.params.toSeq)
//          println("params: " + e.params.map(memory.read(_)).toSeq)
//          println("Value: " + e.eval(memory))
//          println("Gradient: " + gradj(x))
//        }
        val dfij = gradj(x)
        jac(i, j) = dfij

        x += 1
      }
      for(p <- e.params) {}
    }
    jac.build
  }

  def solveLinear: RWMemory = {
    val zeroMem = new MemImpl(norm = _ => NoNormalize)
    val J = jacobian(zeroMem)

    // residuals at 0
    val x = evalResiduals(zeroMem).toArray

//    println("\n Residuals at 0")
//    println(x.toSeq)
//
//    println("\nJacobian")
//    J.print()

    val X = Matrix.fromArray(x)
    val lhs = J.T * J
    val rhs = J.T * X * (-1)

    //  println("\n J.T * J")
    //  lhs.print()
    //
    //  println("\n - J.T * r")
    //  rhs.print()

    // solution of J.T * J * sol = -J.T * x
    val sol = lhs.solveCholSol(rhs.toVector)
    println("\nSol: "); Matrix.fromArray(sol).print()
    zeroMem.load(sol)
    zeroMem
//    println("\nRes: "); Matrix.fromArray(ls.evalResiduals(mem).toArray).print()
  }

  def gaussNewtonIteration(mem: RWMemory): Unit = {
    val J = jacobian(mem)

    // residuals at 0
    val x = evalResiduals(mem).toArray
    val X = Matrix.fromArray(x)
    println("\nprev residuals: ")
    X.print()
    println("\nJacobian:")
    J.print()
    val lhs = J.T * J
    val rhs = J.T * X * (-1)

    println("\nLHS:")
    lhs.print()
    println("\nRHS:")
    rhs.print()

    val update = lhs.solveCholSol(rhs.toVector)
    println("\nUpdate: " + update.mkString("\t"))
    mem.add(update)
  }

  class LMIterator(val tau: Double,
                   val goodUpdate: Double,
                   val errorLimit: Double,
                   mem: RWMemory,
                   verbose: Boolean = false) {
    def println(str: String): Unit = if(verbose) scala.Predef.println(str)

    def this(mem: RWMemory) = this(tau = 0.00001, goodUpdate = 0.5, errorLimit = 1e-7, mem)

    var lambda: Double = -1
    var lastGood = mem.dump

    // sum of residuals
    private var lastChi = -1.0
    private var maxResidual = -1.0
    var ni = 2

    var numIters = 0

    def shouldContinue: Boolean = lastChi < 0 || lastChi > errorLimit

    def stats: SolverStats = SolverStats(maxResidual, numIters)

    def next(): Unit = {
      numIters += 1
      println(s"\n----- New iteration $numIters -----")
      val residuals = evalResiduals(mem).toArray
      println("Residuals: " + residuals.mkString(" "))
      lastGood = mem.dump
      lastChi = residuals.iterator.map(x => x * x).sum
      maxResidual = residuals.iterator.map(math.abs).max
      val J = jacobian(mem)

      if(lambda < 0) {
        // initialize lambda
        lambda = tau * J.max
      }

//      println(s"residuals: ${residuals.mkString(" -- ")}")
//      println("J: ")
//      J.print()
//      println("J.T * J")
//      (J.T * J).print()

      val lhs = J.T * J + Matrix.diagonal(J.n, lambda)
      val rhs = J.T * residuals.map(_ * (-1.0))
      val update =
        Try(lhs.solveCholSol(rhs))
          .orElse(Try(lhs.solveQR(rhs)))
          .getOrElse(sys.exit(1))

      def finite(d: Double): Boolean = {
        !d.isNaN && !d.isInfinity
      }
      val safeUpdate = update.map(v => if(v.isNaN) 0.0 else v)

      mem.add(safeUpdate)
      val newResiduals = evalResiduals(mem).toArray
      val newChi = newResiduals.map(x => x * x).sum
      val improvement = lastChi - newChi

      println(s"Update: ${update.mkString("\t")}")
      println(s"New residuals: ${newResiduals.mkString("\t")}")

      if(finite(improvement) && improvement > 0) {
        lambda *= goodUpdate
        ni = 2
        lastGood = mem.dump
        Stats.numSuccess += 1
        println(s"Improvement: $lastChi -->  $newChi       -- lambda: $lambda")
      } else {
        lambda *= ni
        ni *= 2
        mem.load(lastGood)
        Stats.numFailed += 1
        println(s"Deterioration: $lastChi -> $newChi       -- lambda:  $lambda")
      }
    }

  }

  def lmIterator(mem: RWMemory): LMIterator =
    new LMIterator(mem)

  def lmIteration(mem: RWMemory, numIters: Int): SolverStats = {

    val it = new LMIterator(mem)

    for(i <- 0 until numIters) {
      if(it.shouldContinue)
        it.next()
    }

    it.stats
  }
}

object Stats {
  var numFailed = 0
  var numSuccess = 0
}

case class SolverStats(maxResidual: Double, iterations: Int)
