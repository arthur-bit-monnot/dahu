package dahu.dataframe.metadata


import org.scalatest.FreeSpec
import shapeless._

class MetadataTest extends FreeSpec {

  object StringCol extends ColumMetadata[String, Int, Vector]
  type StringCol = StringCol.type
  case object SymbolColumn extends ColumMetadata[Symbol, Float, Vector]
  type SymbolColumn = SymbolColumn.type

  Key[StringCol]
  Value[StringCol]


  implicitly[ColumnMeta[String,  StringCol :: SymbolColumn :: HNil]]
  implicitly[ColumnMeta[Symbol,  StringCol :: SymbolColumn :: HNil]]
  implicitly[ColumnMeta[String,  SymbolColumn :: StringCol :: HNil]]
  implicitly[ColumnMeta[Symbol,  SymbolColumn :: StringCol :: HNil]]

  "frame-metadata" - {
    "index" in {
      assert(ReverseIndexOfKey[String, StringCol :: SymbolColumn :: HNil].apply() == 1)
      assert(ReverseIndexOfKey[Symbol, StringCol :: SymbolColumn :: HNil].apply() == 0)
    }
  }

}
