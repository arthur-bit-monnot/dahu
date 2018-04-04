package dahu

package object utils {

  def allCombinations[T](inputSeq: Seq[Set[T]]): Set[List[T]] = {
    inputSeq.toList match {
      case Nil           => Set(Nil)
      case values :: Nil => values.map(List(_))
      case values :: tail =>
        allCombinations(tail).flatMap(tailComb => values.map(v => v :: tailComb))
    }
  }

}
