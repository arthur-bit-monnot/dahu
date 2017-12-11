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
    type X = X.type
    type Y = Y.type

    val df = DataFrame.empty
    val df2 = df
      .withColumn(X, Vector(1,2,3))
      .withColumn(Y, Vector("a", "b", "c"))

    assert(df2.indexOf[X] == 0)
    assert(df2.indexOf[Y] == 1)


    println(df2)
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
