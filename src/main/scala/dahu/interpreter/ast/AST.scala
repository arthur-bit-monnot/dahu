package dahu.interpreter.ast

import dahu.dataframe._
import dahu.dataframe.metadata.ColumMetadata
import dahu.dataframe.vector.IndexedVector
import dahu.interpreter._
import dahu.interpreter.ast.Columns.{CGraphColumn, CGraphKey, CodeKey}
import shapeless.HList

import scala.util.Try

object Columns {

  final case object CodeKey
  type CodeKey = CodeKey.type

  type CodeColumn = ColumMetadata[CodeKey, Expr, IndexedVector]

  final case object CGraphKey
  type CGraphKey = CGraphKey.type

  type CGraphColumn = ColumMetadata[CGraphKey, Set[VarID], Vector]

}

final case class ComputationGraph(code: Code) {
  val varFunEdges: Map[VarID, Set[VarID]] =
    code.forward.zipWithIndex
      .collect {
        case (x: Fun, id) => x.args.map(varID => (varID, id)).toVector
      }
      .flatten
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)
}

final case class AST(head: VarID, private val inputCode: IndexedSeq[Expr]) extends ColumnContainer {

  val code: IndexedVector[Expr] = IndexedVector.from(inputCode.toVector)
  val dependencyGraph: Vector[Set[VarID]] = {
    val x = code.vector.zipWithIndex
      .collect {
        case (x: Fun, id) => x.args.map(varID => (varID, id)).toVector
      }
      .flatten
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)

    val arr: Array[Set[VarID]] = Array.fill(code.vector.size)(Set())
    for((k, v) <- x) {
      arr(k) = v
    }
    arr.toVector
  }

  override def toString: String =
    s"AST: $head\n" + code.vector.zipWithIndex
      .map { case (e, i) => s"$i -> $e" }
      .mkString("  ", "\n  ", "")

  lazy val inputs: Seq[Input]      = code.vector.collect { case x: Input => x }
  def address(i: Expr): Res[VarID] =
    code.index.get(i).toRight(Err(s"No expr $i"))

  def at(address: VarID): Res[Expr] = Try(code.vector(address)).toEither

  def funAt(address: FunID): Res[Fun] = at(address) match {
    case valid @ Right(x: Fun) => Right(x)
    case Right(x)              => Left(Err(s"Expected a Fun got: $x"))
    case Left(x)               => Left(x)
  }

  def inputOf(name: String): Res[Input] =
    inputs
      .collectFirst { case x if x.name == name => x }
      .toRight(Err(s"Found no input named $name"))
}

object AST {
  object ExprKey
  type ExprKey = ExprKey.type

  object Dependencies
  type Dependencies = Dependencies.type

  implicit def exprColumn: WithColumn.Aux[ExprKey, Expr, IndexedVector, AST] =
    WithColumn.extractColumn[ExprKey, Expr, IndexedVector, AST](
      _.code,
      (d, vs) => ??? // TODO provide encoding for immutable columns
    )

  implicit def depColumn: WithColumn.Aux[Dependencies, Set[VarID], Vector, AST] =
    WithColumn.extractColumn[Dependencies, Set[VarID], Vector, AST](
      _.dependencyGraph,
      (d, vs) => ??? // TODO provide encoding for immutable columns
    )

}
