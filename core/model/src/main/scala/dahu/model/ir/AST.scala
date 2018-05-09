package dahu.model.ir

import dahu.maps.{ArrayMap, SInt, SubInt, SubSubInt}
import dahu.model.types.Value

trait GenAST[F[_]] {
  trait SubTag
  type ID <: SubInt
  type VID = SubSubInt[ID, GenAST.VariableTag]

  def tree: ArrayMap.Aux[ID, F[ID]]
  def root: ID

  lazy val reverseTree: Map[F[ID], ID] =
    tree.toIterable.map(_.swap).toMap

  type Assignment = VID => Value
  type PartialAssignment = VID => Option[Value]
  type DefaultDomain = VID => Stream[Value]
  type Assignments = Stream[Assignment]
}
object GenAST {
  trait VariableTag
}

trait TotalSubAST[PID <: SubInt] extends GenAST[Total] {

  val subset: TotalSubAST.SubSet[PID, ID]

  lazy val variables: ArrayMap.Aux[VID, InputF[ID]] =
    tree
      .collect { case x: InputF[ID] => x }
      .castKey[VID]

  def asVariableID(id: ID): Option[VID] =
    if(variables.isInDomain(id)) Some(id.asInstanceOf[VID]) else None
}
object TotalSubAST {
  trait SubSet[A, B] {
    def to: A => Option[B]
    def from: B => Option[A]
  }
}

trait AST[T] extends GenAST[ExprF] {

  type Expr = ExprF[ID]

  lazy val variables: ArrayMap.Aux[VID, InputF[ID]] =
    tree
      .collect { case x: InputF[ID] => x }
      .castKey[VID]

  def fromInput: T => Option[ID]
  def toInput: ID => Seq[T]

}
object AST {
  type Aux[T, ID0 <: SubInt] = AST[T] { type ID = ID0 }
}

class ASTImpl[K <: SubInt, T](override val tree: ArrayMap.Aux[K, ExprF[K]],
                              override val root: K,
                              override val fromInput: T => Option[K],
                              override val toInput: K => Seq[T])
    extends AST[T] {
  override type ID = K
}
