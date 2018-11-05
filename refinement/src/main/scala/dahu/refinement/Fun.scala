package dahu.refinement

import common._

trait Fun {

  def numParams: Int

  def bind(addresses: Addr*): RefExpr = bindArray(addresses.toArray)
  def bindArray(params: Array[Addr]): RefExpr = new ExprImpl(this, params)
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
  }

  def gradientAt(x: Values): Array[R] = {
    val board = new Array[R](numParams)
    writeGradient(x, board)
    board
  }

}
