package dahu.refinement

import common._
import dahu.model.input.TypedIdent

import scala.util.Random

trait Memory {
  def size: Int
}

trait RMemory extends Memory {
  def read(addr: Addr): R
  def dump: Values
  def print(): Unit

  def addressOf(name: TypedIdent): Addr

}
trait WMemory extends Memory {
  def write(addr: Addr, value: R): Unit
  def load(values: Array[R]): Unit
  def add(values: Array[R]): Unit
}
trait RWMemory extends RMemory with WMemory

final class MemImpl() extends RWMemory {

  private val mem = debox.Buffer[R]()
  def size: Int = mem.length

//  private val mem: Values = new Array[R](size)
  private val map = debox.Map[TypedIdent, Addr]()

  override def write(addr: Addr, value: R): Unit = mem(addr) = value
  override def read(addr: Addr): R = mem(addr)
  override def dump: Values = mem.toArray()
  override def load(values: Values): Unit = {
    values.indices.foreach(
      i =>
        if(mem.length == i)
          mem.insert(i, values(i))
        else
          mem(i) = values(i))
  }
  override def add(values: Array[R]): Unit = {
    values.indices.foreach(i => mem(i) += values(i))
  }
  override def print(): Unit =
    map.foreach((id, addr) => println(s"$id -> ${mem(addr)}"))
//    mem.foreach(println)

  override def addressOf(name: TypedIdent): Addr = {
    if(!map.contains(name)) {
      map(name) = size
      mem += 0.0 //(Random.nextDouble() - 0.5) * 100
    }
    map(name)
  }
}
