package dahu.refinement

import common._

trait Fun { self =>

  def scale(v: R): Fun = new Fun {
    override def numParams: Addr = self.numParams
    override def eval(params: Values): R = self.eval(params) * v
  }

  def numParams: Int

  def bindSimple(addresses: Addr*): RefExpr = bindArray(addresses.toArray, gradientFactors = null)
  def bindArray(params: Array[Addr], gradientFactors: Array[R]): RefExpr =
    new ExprImpl(this, params, gradientFactors)
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
    }
//    println(s"$center -> ${output.mkString(" ")}")
  }

  def gradientAt(x: Values): Array[R] = {
    val board = new Array[R](numParams)
    writeGradient(x, board)
    board
  }

}

class MinimizationFun(target: R, constantError: R) extends Fun {
  override def numParams: Int = 1
  override def eval(params: Values): R = {
    val v = params(0)
    val r = math.abs(v - target) * constantError
    r
//    constantError
  }

  override def writeGradient(values: Values, output: Array[R]): Unit = {
    output(0) = 1.0 / 20
  }

}
