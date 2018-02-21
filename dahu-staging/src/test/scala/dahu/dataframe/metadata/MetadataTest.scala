package dahu.dataframe.metadata

import org.scalatest.FreeSpec
import shapeless._

class MetadataTest extends FreeSpec {

  import dahu.dataframe.utils.Keys._

  type XYList = XIntCol :: YFloatCol :: HNil
  type YXList = YFloatCol :: XIntCol :: HNil
  type XXLIst = XIntCol :: XIntCol :: HNil

  "frame-metadata" - {
    "column metadata implicit key extraction" in {
      val k: Key.Aux[XIntCol, X] = Key[XIntCol]
      val v: Value.Aux[XIntCol, Int] = Value[XIntCol]
      val c: Container.Aux[XIntCol, Vector] = Container[XIntCol]
    }
    "column extraction by key" in {
      val m11: ColumnMeta.Aux[X, XYList, XIntCol] = ColumnMeta[X, XYList]
      val m21: ColumnMeta.Aux[Y, XYList, YFloatCol] = ColumnMeta[Y, XYList]

      val m12: ColumnMeta.Aux[X, YXList, XIntCol] = ColumnMeta[X, YXList]
      val m22: ColumnMeta.Aux[Y, YXList, YFloatCol] = ColumnMeta[Y, YXList]

      assertDoesNotCompile("ColumnMeta[Y, XXLIst]")
      assertDoesNotCompile("ColumnMeta[X, XXLIst]")
    }
    "index lookup by key" in {
      assert(ReverseIndexOfKey[X, XYList].apply() == 1)
      assert(ReverseIndexOfKey[Y, XYList].apply() == 0)

      assert(ReverseIndexOfKey[X, YXList].apply() == 0)
      assert(ReverseIndexOfKey[Y, YXList].apply() == 1)

      assertDoesNotCompile("ReverseIndexOfKey[X, XXList]")
      assertDoesNotCompile("ReverseIndexOfKey[Y, XXList]")
      assertDoesNotCompile("ReverseIndexOfKey[Z, XYList]")
    }
  }

}
