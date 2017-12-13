package dahu.dataframe.utils

import org.scalatest.FreeSpec

class UtilsTests extends FreeSpec {

  "dataframe-utils" - {

    "reverse-index-of" in {
      import shapeless._

      object X
      type X = X.type
      object Y
      type Y = Y.type
      object Z
      type Z = Z.type

      type L = Z :: Y :: X :: HNil
      val wx: ReverseIndexOf.Aux[X, L, _0] = ReverseIndexOf[X, L]
      val wy: ReverseIndexOf.Aux[Y, L, Succ[_0]] = ReverseIndexOf[Y, L]
      val wz: ReverseIndexOf.Aux[Z, L, Succ[Succ[_0]]] = ReverseIndexOf[Z, L]

      assertDoesNotCompile("ReverseIndexOf[Z, Y :: X :: HNil]")
      assertDoesNotCompile("ReverseIndexOf[X, X :: X :: HNil]")
    }
  }
}
