package dahu.dataframe.errors

sealed abstract class InconsistentDataFrame(msg: String) extends Throwable(msg)

final case class ColumnsOfDifferentSizes(msg: String) extends InconsistentDataFrame(msg)

sealed abstract class IndexingError(msg: String) extends InconsistentDataFrame(msg)

final case class KeyDuplication[A](key: A)
    extends IndexingError(s"Key $key is duplicated in an indexed vector")

final case class KeyMissing[A](key: A)
    extends IndexingError(
      s"Indexing error: value $key is not present in the index an indexed vector")

final case class KeyWronglyIndexed[A](key: A)
    extends IndexingError(
      s"Indexing error: value $key is not present in the index an indexed vector")
