package dahu.dataframe

import dahu.dataframe._
import dahu.dataframe.metadata.IndexOf
import org.scalatest.FreeSpec
import shapeless.ops.hlist.Length
import shapeless.ops.tuple.ToArray

class Test extends FreeSpec {

  import shapeless._

  "init-and-append" in {
    object X
    object Y
    object Z
    type X = X.type
    type Y = Y.type
    type Z = Z.type

    val df = DataFrame.empty
    val df2 = df
      .withColumn(X, Vector(1, 2, 3))
      .withColumn(Y, Vector("a", "b", "c"))

    assert(df2.indexOf[X] == 0)
    assert(df2.indexOf[Y] == 1)

//    df2(X)
    assert(df2(X).values == Vector(1, 2, 3))
    assert(df2(Y).values == Vector("a", "b", "c"))

    val df3 = df2(X).updated(0, 10)
    val xx: Vector[Int] = df3(X).values
    val yy: Vector[String] = df3(Y).values
    assert(df3(X).values == Vector(10, 2, 3))
    assert(df2(Y).values == df3(Y).values)

    assert(df3(X).swapped(Vector(1,2,3)).cols == df2.cols)

    assert(df3(X).get(0) == 10)
    assert(df3(X).get(1) == 2)
    assert(df3(X).get(2) == 3)

    val times10Col = df2(X).values.map(_ * 10)
    val df4 = df3.withColumn(Z, times10Col)

    val tmp: Vector[Int] = df4(Z).values
    assert(df4(Z).values == Vector(10, 20, 30))

//    val df2 = df
//      .append("a" :: 1 :: HNil)
//      .append("b" :: 2 :: HNil)
//
//    assert(df2.cols == Vector(Vector("a", "b"), Vector(1, 2)))
//
//    val df3 = df2(StringCol).updated(0, "A")
//    assert(df3.cols == Vector(Vector("A", "b"), Vector(1, 2)))
//
//    val df4 = df3(IntCol).updated(1, 10)
//    assert(df4.cols == Vector(Vector("A", "b"), Vector(1, 10)))
//
//    val df5 = df4.withColumn("qsdsq", Vector(1.5, 1.3))
//    assert(
//      df5.cols == Vector(Vector(1.5, 1.3), Vector("A", "b"), Vector(1, 10)))
//
//    assert(df5(StringCol).values == Vector("A", "b"))
//    assert(df5(IntCol).get(1) == 10)
//    println(df)
//    println(df2)
//    println(df2.column(StringCol))
  }

}
