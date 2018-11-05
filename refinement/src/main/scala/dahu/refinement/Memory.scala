package dahu.refinement

import common._

trait Memory {
  def size: Int
}

trait RMemory extends Memory {
  def read(addr: Addr): R
  def dump: Values
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
  override def dump: Values = mem.clone()
  override def load(values: Values): Unit =
    values.indices.foreach(i => mem(i) = values(i))
  override def add(values: Array[R]): Unit = {
    values.indices.foreach(i => mem(i) += values(i))
  }
  override def print(): Unit =
    mem.foreach(println)
}
