package dahu.refinement

import common._

trait RefExpr {
  def params: Params
  def eval(mem: RMemory): R
  def gradient(mem: RMemory): Values
}

final class ExprImpl(fun: Fun, val params: Array[Addr]) extends RefExpr {
  val inputSize: Int = params.length
  assert(fun.numParams == params.length)

  protected def loadValues(mem: RMemory, board: Values): Unit = {
    assert(board.length >= inputSize)
    var i = 0
    while(i < inputSize) {
      board(i) = mem.read(params(i))
      i += 1
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
