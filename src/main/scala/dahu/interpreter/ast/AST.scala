package dahu.interpreter.ast

import dahu.dataframe.{DataFrame, RowNumber, WithColumn}
import dahu.dataframe.metadata.ColumMetadata
import dahu.dataframe.vector.IndexedVector
import dahu.interpreter._
import shapeless.HList

import scala.util.Try

object Columns {
  type DF[X <: HList] = DataFrame[X]

  final case object CodeKey
  type CodeKey = CodeKey.type

  type CodeColumn = ColumMetadata[CodeKey, Expr, IndexedVector]

  final case object CGraphKey
  type CGraphKey = CGraphKey.type

  type CGraphColumn = ColumMetadata[CGraphKey, Set[VarID], Vector]

  import shapeless.{::, HList}
  def withComputationGraph[M <: HList](df: DF[M])(
      implicit withColumn: WithColumn[CodeKey, Expr, M]): DF[CGraphColumn :: M] = {
    val x = df(CodeKey).values.zipWithIndex
      .collect {
        case (x: Fun, id) => x.args.map(varID => (varID, id)).toVector
      }
      .flatten
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)

    val arr: Array[Set[VarID]] = Array.fill(withColumn.size(df))(Set())
    for((k, v) <- x) {
      arr(k) = v
    }
    df.withColumn(CGraphKey, arr.toVector)
  }
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

final case class AST(head: VarID, code: Code) {
  override def toString: String =
    s"AST: $head\n" + code.forward.zipWithIndex
      .map { case (e, i) => s"$i -> $e" }
      .mkString("  ", "\n  ", "")

  lazy val inputs: Seq[Input] = code.forward.collect { case x: Input => x }.toSeq
  def address(i: Expr): Res[VarID] =
    code.backward.get(i).toRight(Err(s"No expr $i"))

  def at(address: VarID): Res[Expr] = Try(code.forward(address)).toEither

  def funAt(address: FunID): Res[Fun] = at(address) match {
    case valid @ Right(x: Fun) => Right(x)
    case Right(x)              => Left(Err(s"Expected a Fun got: $x"))
    case Left(x)               => Left(x)
  }

  def inputOf(name: String): Res[Input] =
    inputs
      .collectFirst { case x if x.name == name => x }
      .toRight(Err(s"Found no input named $name"))

  lazy val graph = ComputationGraph(code)
}
