package dahu.refinement.la

import dahu.matrix.{Matrix, MatrixFactory}
import spire.syntax.cfor._

object common {
  type Addr = Int
  type R = Double

  type Params = Array[Addr]
  type Values = Array[R]

  /** Infinitesimal change for computing finite differences.
    * Equal sqrt(10**-16) = 10**-8 (see Numerical Optimization Chap 7.1 for explanation)  */
  val epsilon: R = 0.00001
}
import common._

trait Memory {
  def size: Int
}

trait RMemory extends Memory {
  def read(addr: Addr): R
  def print(): Unit
}
trait WMemory extends Memory {
  def write(addr: Addr, value: R): Unit
  def load(values: Array[R]): Unit
  def add(values: Array[R]): Unit
}
trait RWMemory extends RMemory with WMemory

final class MemImpl(val size: Int) extends RWMemory {

  private val mem: Values = new Array[R](size)

  override def write(addr: Addr, value: R): Unit = mem(addr) = value
  override def read(addr: Addr): R = mem(addr)
  override def load(values: Array[R]): Unit =
    values.indices.foreach(i => mem(i) = values(i))
  override def add(values: Array[R]): Unit = {
    values.indices.foreach(i => mem(i) += values(i))
  }
  override def print(): Unit =
    mem.foreach(println)
}

trait Expr {
  def params: Params
  def eval(mem: RMemory): R
  def gradient(mem: RMemory): Values
}

trait Fun {

  def numParams: Int

  def bind(addresses: Addr*): Expr = bindArray(addresses.toArray)
  def bindArray(params: Array[Addr]): Expr = new ExprImpl(this, params)
  def eval(params: Values): R

  def writeGradient(values: Values, output: Array[R]): Unit = {
    val center: R = eval(values)
    var i = 0
    while(i < numParams) {
      values(i) += epsilon
      val fi = eval(values)

      val dfi = (fi - center) / epsilon
      output(i) = dfi
      values(i) -= epsilon
      i += 1

      if(i == 2)
        println("break")

    }
  }

  def gradientAt(x: Values): Array[R] = {
    val board = new Array[R](numParams)
    writeGradient(x, board)
    board
  }

}

final class ExprImpl(fun: Fun, val params: Array[Addr]) extends Expr {
  val inputSize: Int = params.length
  assert(fun.numParams == params.length)

  protected def loadValues(mem: RMemory, board: Values): Unit = {
    assert(board.length >= inputSize)
    cfor(0)(_ < inputSize, _ + 1) { i =>
      board(i) = mem.read(params(i))
    }
  }

  def evalWithBoard(mem: RMemory, board: Values): R = {
    loadValues(mem, board)
    fun.eval(board)
  }

  override def eval(mem: RMemory): R = {
    val board: Values = new Array[R](params.length)
    evalWithBoard(mem, board)
  }
  override def gradient(mem: RMemory): Values = {
    val values: Values = new Array[R](params.length)
    loadValues(mem, values)
    fun.gradientAt(values)
  }
}

class LeastSquares(residuals: Seq[Expr]) {

  val numVars: Int = residuals.map(_.params.max).max

  def evalResiduals(memory: RMemory): Seq[R] = {
    residuals.map(_.eval(memory))
  }

  def jacobian(memory: RMemory): Matrix = {
    val jac = new MatrixFactory
    println("\nGradients")
    for((e, i) <- residuals.zipWithIndex) {
      val gradj = e.gradient(memory)
      println(gradj.mkString("\t"))
      var x = 0
      while(x < e.params.length) {
        val j = e.params(x)
        val dfij = gradj(x)
        jac(i, j) = dfij
        x += 1
      }
      for(p <- e.params) {}
    }
    jac.build
  }

  def solveLinear: RWMemory = {
    val zeroMem = new MemImpl(numVars)
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
    val sol = lhs.solve(rhs.toVector)
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

    val update = lhs.solve(rhs.toVector)
    println("\nUpdate: " + update.mkString("\t"))
    mem.add(update)
  }
}

object Test extends App {

  val mem: RWMemory = new MemImpl(4)
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
  def at(x: Addr, y: Addr, d: Int): Expr = {
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

  val zeroMem = new MemImpl(4)

  for(i <- 0 until 10) {
    println(s"\n\n ----- Iter $i ------_n")
    ls.gaussNewtonIteration(zeroMem)
    zeroMem.print()
    println(ls.evalResiduals(zeroMem))
  }

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
