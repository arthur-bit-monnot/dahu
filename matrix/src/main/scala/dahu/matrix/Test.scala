package dahu.matrix

import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcs
import edu.emory.mathcs.csparsej.tdouble._

import scala.collection.mutable

class MatrixFactory {
  private val M: Dcs = Dcs_util.cs_spalloc(0, 0, 1, true, true)
  private val entries = new mutable.LongMap[Null]()

  private def merge(i: Int, j: Int): Long = i.toLong + (j.toLong << 32)

  def update(i: Int, j: Int, value: Double): Unit = {
    require(!entries.contains(merge(i, j)), s"duplicate entry: ($i, $j)")
    entries.update(merge(i, j), null)
    Dcs_entry.cs_entry(M, i, j, value)
  }

  def build: Matrix = new Matrix(Dcs_compress.cs_compress(M))
}

final class Vector(data: Array[Double]) {
  def this(size: Int) = this(new Array[Double](size))

  val size = data.length
  def apply(i: Int): Double = data(i)
  def update(i: Int, v: Double): Unit = data(i) = v
}

class Matrix(private val M: Dcs) { lhs =>
  require(Dcs_util.CS_CSC(M))

  def n: Int = M.n
  def m: Int = M.m

  def T: Matrix = new Matrix(Dcs_transpose.cs_transpose(M, true))

  def *(rhs: Matrix): Matrix = {
    require(lhs.n == rhs.m, "Dimensions do not match for multiplication")
    new Matrix(Dcs_multiply.cs_multiply(M, rhs.M))
  }
  def *(rhs: Array[Double]): Array[Double] = {
    (this * Matrix.fromArray(rhs)).toVector
  }
  def solve(b: Array[Double]): Array[Double] = {
    val x = b.clone()
    val res = Dcs_cholsol.cs_cholsol(0, M, x)
//    val res = Dcs_qrsol.cs_qrsol(0, M, x)
    assert(res)
    x
  }
  def *(factor: Double): Matrix = {
    val fm = new Dcs_common.Dcs
    fm.i = M.i.clone()
    fm.m = M.m
    fm.n = M.n
    fm.nz = M.nz
    fm.nzmax = M.nzmax
    fm.p = M.p.clone()
    fm.x = M.x.map(_ * factor)
    new Matrix(fm)
  }

  def norm1: Double = {
    val res = Dcs_norm.cs_norm(M)
    assert(res >= 0)
    res
  }

  def toVector: Array[Double] = {
    require(n == 1)
    val res = new Array[Double](m)
    foreachValue((i, j, v) => res(i) = v)
    res
  }

  def foreachValue(f: (Int, Int, Double) => Unit): Unit = {
    var col = 0
    while(col < n) {
      var p = M.p(col)
      while(p < M.p(col + 1)) {
        val row = M.i(p)
        val value = if(M.x == null) 1d else M.x(p)
        f(row, col, value)
        p += 1
      }
      col += 1
    }
//    for (j = 0; col < n; col++) {
//                System.out.print(String.format("    col %d : locations %d to %d\n", col, Ap[j], Ap[j + 1] - 1));
//                for (p = Ap[j]; p < Ap[j + 1]; p++) {
//                    System.out.print(String.format("      %d : %g\n", Ai[p], Ax != null ? Ax[p] : 1));
//                    if (brief && p > 20) {
//                        System.out.print("  ...\n");
//                        return (true);
//                    }
//                }
//            }
  }

  def print(brief: Boolean = false): Unit = Dcs_print.cs_print(M, brief)
}

object Matrix {
  def fromArray(arr: Array[Double]): Matrix = {
    val fac = new MatrixFactory
    arr.indices.foreach(i => fac(i, 0) = arr(i))
    fac.build
  }

}

object Test extends App {

  val T: Dcs = Dcs_util.cs_spalloc(0, 0, 1, true, true)

  val fac = new MatrixFactory
  fac(1, 1) = 20
  fac(1, 2) = 10

  val m = fac.build
  m.print()
  println(m.norm1)

  m.T.print()

  (m * m.T).print()

//
//  Dcs_entry.cs_entry(T, 1, 1, 5)
//  Dcs_entry.cs_entry(T, 2, 2, 10)
//  Dcs_entry.cs_entry(T, 1, 1, 10)
//  val U = Dcs_compress.cs_compress(T)
//
//  Dcs_print.cs_print(T, true)
//  println("compress: " + Dcs_util.CS_CSC(T))
//  Dcs_print.cs_print(U, true)
//  println("compress: " + Dcs_util.CS_CSC(U))
}
