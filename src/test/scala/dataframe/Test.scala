package dataframe

import dahu.dataframe.{DataFrame, MetaData}
import org.scalatest.FreeSpec
import shapeless.ops.hlist.Length
import shapeless.ops.tuple.ToArray

class Test extends FreeSpec {


  import shapeless._

  "init-and-append" in {
    type H = String :: Int :: HNil
    Length[H]
    MetaData[String :: Int :: HNil]

    val df = new DataFrame[String :: Int :: HNil](Vector.fill(2)(Vector[Any]()))

    val df2 = df
      .append("A" :: 1 :: HNil)
      .append("B" :: 2 :: HNil)
      .append("C" :: 3 :: HNil)
    println(df)
    println(df2)
    println(df2.column("fsdf"))
  }

}
