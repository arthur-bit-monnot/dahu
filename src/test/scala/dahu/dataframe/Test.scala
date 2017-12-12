package dahu.dataframe

import dahu.dataframe._
import dahu.dataframe.metadata.{:::, ColMeta, EmptyFrame, IndexOf}
import dahu.dataframe.vector.IndexedVector
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
    val df2: DataFrame[ColMeta.Aux[Y, String, Vector] ::: ColMeta.Aux[X, Int, Vector] ::: EmptyFrame] = df
      .withColumn(X, Vector(1, 2, 3))
      .withColumn(Y, Vector("a", "b", "c"))

    assert(df2.indexOf[X] == 0)
    assert(df2.indexOf[Y] == 1)

//    df2(X)
    assert(df2(X).values == Vector(1, 2, 3))
    assert(df2(Y).values == Vector("a", "b", "c"))

    val df3: DataFrame[ColMeta.Aux[Y, String, Vector] ::: ColMeta.Aux[X, Int, Vector] ::: EmptyFrame] = df2(X).updated(0, 10)
//    val df3 = df2.apply(X).updated(0, 10)
    val sqdsqd: Column[Int, Vector, ColMeta.Aux[Y, String, Vector] ::: ColMeta.Aux[X, Int, Vector] ::: EmptyFrame] = df2.apply(X)
    val xx: Vector[Int] = df3(X).values
    val yy: Vector[String] = df3(Y).values
    assert(df3(X).values == Vector(10, 2, 3))
    assert(df2(Y).values == df3(Y).values)

//    assert(df3(X).swapped(Vector(1,2,3)).cols == df2.cols)
//
//    assert(df3(X).get(0) == 10)
//    assert(df3(X).get(1) == 2)
//    assert(df3(X).get(2) == 3)
//
//    val times10Col = df2(X).values.map(_ * 10)
//    val df4 = df3.withColumn(Z, times10Col)
//
//    val tmp: Vector[Int] = df4(Z).values
//    assert(df4(Z).values == Vector(10, 20, 30))
//
////    val xIndexed: DataFrame[ColMeta.Aux[Y, String, IndexedVector] ::: ColMeta.Aux[X, Int, Vector] ::: EmptyFrame] = df2.indexed(Y)
//    val xIndexed = df2.indexed(Y)
//    val yIndexed = df2.indexed(Y)
//    val colY: Column[String, IndexedVector, _] = yIndexed.apply(Y)
//    val qsdsqd: IndexedVector[String] = colY.values
//    assert(yIndexed(X).values == Vector(1, 2, 3))
//    assert(yIndexed(Y).values.v == Vector("a", "b", "c"))

  }

}
