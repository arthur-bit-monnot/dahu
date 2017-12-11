package dahu.dataframe.metadata


import org.scalatest.FreeSpec

class MetadataTest extends FreeSpec {

  trait IntCol extends ColMeta {
    override type K = String
    override type V = Int
  }
  trait StringCol extends ColMeta {
    override type K = Symbol
    override type V = String
  }

  implicitly[ColumnMeta[String,  IntCol ::: StringCol ::: EmptyFrame]]
  implicitly[ColumnMeta[Symbol,  IntCol ::: StringCol ::: EmptyFrame]]
  implicitly[ColumnMeta[String,  StringCol ::: IntCol ::: EmptyFrame]]
  implicitly[ColumnMeta[Symbol,  StringCol ::: IntCol ::: EmptyFrame]]

  "frame-metadata" - {
    "index" in {
      assert(IndexOf[String, IntCol ::: StringCol ::: EmptyFrame].apply() == 1)
      assert(IndexOf[Symbol, IntCol ::: StringCol ::: EmptyFrame].apply() == 0)
    }
  }

}
