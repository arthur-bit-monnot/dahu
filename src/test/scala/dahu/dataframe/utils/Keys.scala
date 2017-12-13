package dahu.dataframe.utils

import dahu.dataframe.metadata.ColumMetadata

object Keys {

  object X
  type X = X.type

  object Y
  type Y = Y.type

  object Z
  type Z = Z.type

  case object XIntCol extends ColumMetadata[X, Int, Vector]
  type XIntCol = XIntCol.type

  case object YFloatCol extends ColumMetadata[Y, Float, Vector]
  type YFloatCol = YFloatCol.type
}
