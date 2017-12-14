package dahu.dataframe

import dahu.dataframe.errors.{ColumnsOfDifferentSizes, KeyDuplication}
import dahu.dataframe.vector.IndexedVector
import org.scalatest.FreeSpec

class DataFrameTest extends FreeSpec {

  import utils.Keys._

  val df  = DataFrame.empty
  val df1 = df.withColumn(X, Vector(1, 2, 3))
  val df2 = df1.withColumn(Y, Vector('a, 'b, 'c))

  "dataframe" - {
    "base properties" in {
      assert(df2.raw(X) == Vector(1, 2, 3))
      val yCol = df2(Y)
      val xCol = df2(X)

      assert(df2(X).values == Vector(1, 2, 3))
      assert(df2(Y).values == Vector('a, 'b, 'c))

      assert(df2.indexOf[X] == 0)
      assert(df2.indexOf[Y] == 1)
    }

    "updates" in {
      val df3                    = df2.apply(X).updated(0, 10)
      val typed1: Vector[Int]    = df3.column(X).content
      val typed2: Vector[Symbol] = df3.column(Y).content
      assert(df3(X).values == Vector(10, 2, 3))
      assert(df2(Y).values == df3(Y).values)

      assert(df3.column(X).swapped(Vector(1, 2, 3)).cols == df2.cols)

      assert(df3(X).valueAt(0) == 10)
      assert(df3(X).valueAt(1) == 2)
      assert(df3(X).valueAt(2) == 3)

      val times10Col = df2(X).values.map(_ * 10)
      val df4        = df3.withColumn(Z, times10Col.toVector)

      val typed3: Vector[Int] = df4.column(Z).content
      assert(df4(Z).values == Vector(10, 20, 30))
    }

    "invalid updates" in {
      assertThrows[ColumnsOfDifferentSizes](df2.withColumn(Z, Vector(1)))
    }

    "indexing" in {
      val xIndexed                                = df2.indexed(X)
      val yIndexed                                = df2.indexed(Y)
      val colY: ColumnF[Symbol, IndexedVector, _] = yIndexed.column(Y)
      val qsdsqd: IndexedVector[Symbol]           = colY.content

      assert(yIndexed(X).values == Vector(1, 2, 3))
      assert(yIndexed(Y).values == Vector('a, 'b, 'c))

      val df10 = yIndexed(Y).updated(0, 'd)
      val df11 = df10(Y).updated(1, 'a)

      assert(df11(Y).values == Vector('d, 'a, 'c))

      assertThrows[KeyDuplication[Symbol]](df11(Y).updated(2, 'd))

    }
  }

}
