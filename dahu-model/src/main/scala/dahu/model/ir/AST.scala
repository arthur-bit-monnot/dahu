package dahu.model.ir

import dahu.arrows.ArrayIntFunc

trait AST[T] {

  type ID
  type Expr = ExprF[ID]

  def root: ID
  def tree: ArrayIntFunc.Aux[ID, Expr]

  lazy val variables = tree.filter(_.isInstanceOf[InputF[_]]).map(_.asInstanceOf[InputF[_]])

  def fromInput: T => Option[ID]
}

class ASTImpl[K, T](override val tree: ArrayIntFunc.Aux[K, ExprF[K]],
                    override val root: K,
                    override val fromInput: T => Option[K])
    extends AST[T] {
  override type ID = K
}
