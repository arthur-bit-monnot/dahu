package dahu.dataframe.metadata


import org.scalatest.FreeSpec
import shapeless._

class MetadataTest extends FreeSpec {

  object StringCol extends ColumMetadata[String, Int, Vector]
  type StringCol = StringCol.type
  case object SymbolColumn extends ColumMetadata[Symbol, Float, Vector]
  type SymbolColumn = SymbolColumn.type

  type StringSymList = StringCol :: SymbolColumn :: HNil
  type SymStringList = SymbolColumn :: StringCol :: HNil

  "frame-metadata" - {
    "column metadata implicit key extraction" in {
      val k: Key.Aux[StringCol, String] = Key[StringCol]
      val v: Value.Aux[StringCol, Int] = Value[StringCol]
      val c: Container.Aux[StringCol, Vector] = Container[StringCol]
    }
    "column extraction by key" in {
      val m11: ColumnMeta.Aux[String, StringSymList, StringCol] = ColumnMeta[String, StringSymList]
      val m21: ColumnMeta.Aux[Symbol, StringSymList, SymbolColumn] = ColumnMeta[Symbol, StringSymList]

      val m12: ColumnMeta.Aux[String, SymStringList, StringCol] = ColumnMeta[String, SymStringList]
      val m22: ColumnMeta.Aux[Symbol, SymStringList, SymbolColumn] = ColumnMeta[Symbol, SymStringList]

      implicitly[ColumnMeta[String, StringSymList]]
      implicitly[ColumnMeta[Symbol, StringSymList]]
      implicitly[ColumnMeta[String, SymStringList]]
      implicitly[ColumnMeta[Symbol, SymStringList]]
    }
    "index lookup by key" in {
      assert(ReverseIndexOfKey[String, StringSymList].apply() == 1)
      assert(ReverseIndexOfKey[Symbol, StringSymList].apply() == 0)
    }
  }

}
