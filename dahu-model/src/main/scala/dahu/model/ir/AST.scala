package dahu.model.ir

import dahu.arrows.{ArrayIntFunc, SubInt, SubSubInt}

trait AST[T] {

  type ID <: SubInt
  type Expr = ExprF[ID]
  sealed trait VariableTag
  type VID = SubSubInt[ID, VariableTag]

  def root: ID
  def tree: ArrayIntFunc.Aux[ID, Expr]

  lazy val variables: ArrayIntFunc.Aux[VID, InputF[ID]] =
    tree
      .collect { case x: InputF[ID] => x }
      .castKey[VID]

  def fromInput: T => Option[ID]
}

class ASTImpl[K <: SubInt, T](override val tree: ArrayIntFunc.Aux[K, ExprF[K]],
                              override val root: K,
                              override val fromInput: T => Option[K])
    extends AST[T] {
  override type ID = K
}
