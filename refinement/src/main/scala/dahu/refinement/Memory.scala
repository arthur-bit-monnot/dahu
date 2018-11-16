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
  def copy: RMemory
}
trait WMemory extends Memory {
  def write(addr: Addr, value: R): Unit
  def load(values: Array[R]): Unit
  def add(values: Array[R]): Unit
}
trait RWMemory extends RMemory with WMemory

final class MemImpl(baseSize: Int = 0) extends RWMemory {

  private val mem = debox.Buffer.fill[R](baseSize)(0.1)
  def size: Int = mem.length

  check()

  def check(): Unit =
    for(i <- 0 until size)
      assert(!mem(i).isNaN)

//  private val mem: Values = new Array[R](size)
  private val map = debox.Map[TypedIdent, Addr]()

  override def write(addr: Addr, value: R): Unit = {
    assert(!value.isNaN)
    mem(addr) = value
  }
  override def read(addr: Addr): R = mem(addr)
  override def copy = {
    val newMem = new MemImpl(baseSize = size)
    val vals = dump
    newMem.load(vals)
    check()
    newMem.check()
    newMem
  }
  override def dump: Values = mem.toArray()
  override def load(values: Values): Unit = {
    values.indices.foreach(
      i =>
        if(mem.length == i)
          mem.insert(i, values(i))
        else
          mem(i) = values(i))
    check()
  }
  override def add(values: Array[R]): Unit = {
    values.indices.foreach(i => mem(i) += values(i))
    check()
  }
  override def print(): Unit =
    map
      .iterator()
      .toSeq
      .sortBy(_._1.toString)
      .foreach { case (id, addr) => println(s"$id -> ${mem(addr)}") }
//    mem.foreach(println)

  override def addressOf(name: TypedIdent): Addr = {
    if(!map.contains(name)) {
      map(name) = size
      if(name.toString.endsWith(".dt"))
        mem += 1 //(Random.nextDouble() - 0.5) * 100
      else
        mem += 0.0001
    }
    map(name)
  }
}
