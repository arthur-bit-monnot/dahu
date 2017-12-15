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

final case class AST[Cols <: HList](head: VarID, dataFrame: DF[Cols])(
    implicit withCode: WithColumn[CodeKey, Expr, Cols],
    withIndexedCode: WithIndex[CodeKey, Expr, Cols]
) {
//  private val code  = dataFrame(CodeKey)
//  private val index = IndexedColumn.from(dataFrame, CodeKey)

  override def toString: String = ???
//    s"AST: $head\n" + code.values.zipWithIndex
//      .map { case (e, i) => s"$i -> $e" }
//      .mkString("  ", "\n  ", "")

  lazy val inputs: Seq[Input]      = ??? //code.values.collect { case x: Input => x }.toSeq
  def address(i: Expr): Res[VarID] = ???
//    index.id(i).toRight(Err(s"No expr $i"))

  def at(address: VarID): Res[Expr] = ??? // Try(code.valueAt(address)).toEither

  def funAt(address: FunID): Res[Fun] = at(address) match {
    case valid @ Right(x: Fun) => Right(x)
    case Right(x)              => Left(Err(s"Expected a Fun got: $x"))
    case Left(x)               => Left(x)
  }

  def inputOf(name: String): Res[Input] =
    inputs
      .collectFirst { case x if x.name == name => x }
      .toRight(Err(s"Found no input named $name"))

//  lazy val graph = ComputationGraph(code)
}

object ASTSyntax {

//  implicit class ASTOps[Cols <: HList](val df: DataFrame[Cols])(
//      implicit withCode: WithColumn[CodeKey, Expr, Cols],
//      withIndexedCode: WithIndex[CodeKey, Expr, Cols]
//  ) extends AnyVal {
//    private def code  = df(CodeKey)
//    private def index = IndexedColumn.from(df, CodeKey)
//
//    def inputs: Seq[Input] = code.values.collect { case x: Input => x }.toSeq
//
//    def address(i: Expr): Res[VarID] =
//      index.id(i).toRight(Err(s"No expr $i"))
//
//    def at(address: VarID): Res[Expr] = Try(code.valueAt(address)).toEither
//
//    def funAt(address: FunID): Res[Fun] = at(address) match {
//      case valid @ Right(x: Fun) => Right(x)
//      case Right(x)              => Left(Err(s"Expected a Fun got: $x"))
//      case Left(x)               => Left(x)
//    }
//
//    def inputOf(name: String): Res[Input] =
//      inputs
//        .collectFirst { case x if x.name == name => x }
//        .toRight(Err(s"Found no input named $name"))
//
//    import shapeless.{::, HList}
//    def withComputationGraph(df: DF[Cols]): DF[CGraphColumn :: Cols] = {
//      val x = df(CodeKey).values.zipWithIndex
//        .collect {
//          case (x: Fun, id) => x.args.map(varID => (varID, id)).toVector
//        }
//        .flatten
//        .groupBy(_._1)
//        .mapValues(_.map(_._2).toSet)
//
//      val arr: Array[Set[VarID]] = Array.fill(withCode.size(df))(Set())
//      for((k, v) <- x) {
//        arr(k) = v
//      }
//      df.withColumn(CGraphKey, arr.toVector)
//    }
//  }

}
