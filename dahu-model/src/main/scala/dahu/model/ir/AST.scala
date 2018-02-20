package dahu.model.ir

import dahu.maps.{ArrayMap, SubInt, SubSubInt}

trait AST[T] {

  type ID <: SubInt
  type Expr = ExprF[ID]
  sealed trait VariableTag
  type VID = SubSubInt[ID, VariableTag]

  def root: ID
  def tree: ArrayMap.Aux[ID, Expr]

  lazy val variables: ArrayMap.Aux[VID, InputF[ID]] =
    tree
      .collect { case x: InputF[ID] => x }
      .castKey[VID]

  def fromInput: T => Option[ID]
}
object AST {
  type Aux[T, ID0 <: SubInt] = AST[T] { type ID = ID0}
}

class ASTImpl[K <: SubInt, T](override val tree: ArrayMap.Aux[K, ExprF[K]],
                              override val root: K,
                              override val fromInput: T => Option[K])
    extends AST[T] {
  override type ID = K
}
