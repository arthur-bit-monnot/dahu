package dataframe

import dahu.dataframe._
import org.scalatest.FreeSpec
import shapeless.ops.hlist.Length
import shapeless.ops.tuple.ToArray

class Test extends FreeSpec {


  import shapeless._


  object StringCol {
    implicit val col: Column.Aux[StringCol.type, String] = new Column[StringCol.type] { type Field = String }
  }

  object IntCol {
    implicit val col: Column.Aux[IntCol.type, Int] = new Column[IntCol.type] { type Field = Int }
  }

  "init-and-append" in {
    type H = String :: Int :: HNil
    Length[H]
    the[TypedColumns[HNil]]
    the[TypedColumns.Aux[HNil,HNil]]
    the[Column.Aux[IntCol.type, Int]]
    TypedColumns[IntCol.type :: HNil]
//    MetaData[StringCol.type :: IntCol.type :: HNil]

    val df = DataFrame(StringCol, IntCol)
    val df2 = df
      .append("a" :: 1 :: HNil)
      .append("b" :: 2 :: HNil)

    assert(df2.cols == Vector(Vector("a","b"), Vector(1, 2)))

    val df3 = df2.updated(StringCol, 0, "A")
    assert(df3.cols == Vector(Vector("A","b"), Vector(1, 2)))

    val df4 = df3.updated(IntCol, 1, 10)
    assert(df4.cols == Vector(Vector("A","b"), Vector(1, 10)))

    val df5 = df4.withColumn("qsdsq", Vector(1.5, 1.3))
    assert(df5.cols == Vector(Vector(1.5, 1.3), Vector("A","b"), Vector(1, 10)))

    assert(df5(StringCol).values == Vector("A", "b"))
    assert(df5(IntCol).get(1) == 10)
        println(df)
    println(df2)
//    println(df2.column(StringCol))
  }

}
