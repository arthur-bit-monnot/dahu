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
    val jac = new MatrixFactory
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
        if(!memory.isReadOnly(j)) {
          val dfij = gradj(x)
          jac(i, j) = dfij
        }
        x += 1
      }
      for(p <- e.params) {}
    }
    jac.build
  }

  def solveLinear: RWMemory = {
    val zeroMem = new MemImpl()
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
    var lastChi = -1.0
    var ni = 2

    var numIters = 0

    def shouldContinue: Boolean = lastChi < 0 || lastChi > errorLimit

    def next(): Unit = {
      numIters += 1
      println(s"\n----- New iteration $numIters -----")
      val residuals = evalResiduals(mem).toArray
      println("Residuals: " + residuals.mkString(" "))
      lastGood = mem.dump
      lastChi = residuals.map(x => x * x).sum
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
        println(s"Improvement: $lastChi -->  $newChi       -- lambda: $lambda")
      } else {
        lambda *= ni
        ni *= 2
        mem.load(lastGood)
        println(s"Deterioration: $newChi -- $lambda")
      }
    }

  }

  def lmIterator(mem: RWMemory): LMIterator =
    new LMIterator(mem)

  def lmIteration(mem: RWMemory, numIters: Int): Unit = {

    val it = new LMIterator(mem)

    for(i <- 0 until numIters) {
      if(it.shouldContinue)
        it.next()
    }

  }
}

object Test extends App {

  val mem: RWMemory = new MemImpl()
  val x1: Addr = 0
  val x2: Addr = 1
  val y1: Addr = 2
  val y2: Addr = 3

  mem.write(x1, 0)
  mem.write(x2, 10)
  mem.write(y1, 0)
  mem.write(y2, -5)

  val dist: Fun = new Fun {
    override def numParams: Addr = 2
    override def eval(params: Values): R = params(1) - params(0) + 1
  }
  val at1: Fun = new Fun {
    override def numParams: Addr = 1
    override def eval(params: Values): R = if(params(0) < 1) 0 else params(0) - 1
  }
  def at(x: Addr, y: Addr, d: Int): RefExpr = {
    val f = new Fun {
      override def numParams: Addr = 2
      override def eval(params: Values): R = {
        val x = params(0)
        val y = params(1)
        math.sqrt(x * x + y * y) - d
      }
    }
    f.bind(x, y)
  }
  val distX1X2 = dist.bind(x1, x2)
  val distY1Y2 = dist.bind(y1, y2)
  val distX1Y2 = dist.bind(x1, y2)
  val distY1X2 = dist.bind(y1, x2)

  println(distX1X2.eval(mem))
  println(distX1X2.gradient(mem).toSeq)

  val ls = new LeastSquares(
    Seq(
      at(x1, y1, 100),
      at(x2, y2, 10),
//      distX1X2,
//      distY1Y2,
      at1.bind(x1),
      at1.bind(x2),
//      at1.bind(y1),
////      at1.bind(y2),
//      //    distX1Y2,
////    distY1X2
    ))
  println(ls.evalResiduals(mem))

  //

  val zeroMem = new MemImpl()

  ls.lmIteration(zeroMem, 10)

  zeroMem.print()

//  for(i <- 0 until 10) {
//    println(s"\n\n ----- Iter $i ------_n")
//    ls.gaussNewtonIteration(zeroMem)
//    zeroMem.print()
//    println(ls.evalResiduals(zeroMem))
//  }

//  val J = ls.jacobian(zeroMem)
//  val x = ls.evalResiduals(zeroMem).toArray
////  val x = Array[Double](2, 3)
//
//  println("\n Residuals at 0")
//  println(x.toSeq)
//
//  println("\nJacobian")
//  J.print()
//
////  (J * 2).print()
////  J.print()
//  val X = Matrix.fromArray(x)
////  val JX = (J * X)
////  JX.print()
////  println(JX.toVector.toSeq)
//
////  J.transpose.print()
////  X.print()
//  val lhs = J.T * J
//  val rhs = J.T * X * (-1)
//
//  println("\n J.T * J")
//  lhs.print()
//
//  println("\n - J.T * r")
//  rhs.print()
//
//  val sol = lhs.solve(rhs.toVector)
//  println("\nSol: "); Matrix.fromArray(sol).print()
//  mem.load(sol)
//  println("\nRes: "); Matrix.fromArray(ls.evalResiduals(mem).toArray).print()
//
//  println((J.T * J * sol).toSeq)
//
////  println(
////    lhs.solve(rhs.toVector).toSeq
////  )

}
