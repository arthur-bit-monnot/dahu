package dahu.dataframe

/**
  * Marker class that provides extension methods for subclasses.
  *
  * It is also used as a restriction for adding containers to dataframes.
  * This is mainly to avoid programming mistakes.
  * */
trait ColumnContainer

object ColumnContainer {

  implicit class ColumnContainerOps[D <: ColumnContainer](val d: D) extends AnyVal {
    def apply[K, V](k: K)(implicit wi: WithColumn[K, V, D]): Column[V, D] =
      Column.from(d, k)

    def column[K, V, F[_]](k: K)(implicit wi: WithColumn.Aux[K, V, F, D]): ColumnF[V, F, D] =
      ColumnF.from(d, k)
  }

}
