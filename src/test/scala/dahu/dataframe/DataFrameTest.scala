package dahu.dataframe

import dahu.dataframe.errors.{ColumnsOfDifferentSizes, KeyDuplication}
import dahu.dataframe.vector.{IndexedVector, Vec}
import org.scalatest.FreeSpec

/** Marker class that provides extension methods for subclasses */
trait ColumnContainer

object ColumnContainer {

  implicit class ColumnContainerOps[D <: ColumnContainer](val d: D) extends AnyVal {
    def column[K, V](k: K)(implicit wi: WithColumn[K, V, D]): Column[V, D] =
      Column.from(d, k)
  }

}

class DataFrameTest extends FreeSpec {

  import utils.Keys._

  val df  = DF.empty
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

    "nesting" in {

      case class AB(col1: Vector[Int], col2: Vector[Double]) extends ColumnContainer
      object AB {
        object A
        type A = A.type
        object B
        type B = B.type

        implicit val withColumn1: WithColumn.Aux[A, Int, Vector, AB] =
          WithColumn.extractColumn[A, Int, Vector, AB](A, _.col1, (d, vs) => d.copy(col1 = vs))

        implicit val withColumn2: WithColumn.Aux[B, Double, Vector, AB] =
          WithColumn.extractColumn[B, Double, Vector, AB](B, _.col2, (d, vs) => d.copy(col2 = vs))
      }

      val ab = AB(Vector(1, 2, 3), Vector(1.0, 2.0, 3.0))

      ab.column(AB.A)
      assert(ab.column(AB.A).valueAt(0) == 1)
      assert(ab.column(AB.A).valueAt(1) == 2)
      assert(ab.column(AB.A).valueAt(2) == 3)

      assert(ab.column(AB.B).valueAt(0) == 1.0)
      assert(ab.column(AB.B).valueAt(1) == 2.0)
      assert(ab.column(AB.B).valueAt(2) == 3.0)

    }
  }

}
