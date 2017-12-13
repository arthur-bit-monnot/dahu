package dahu.interpreter.ast

import dahu.interpreter._

import scala.util.Try
import scalaz._
import Scalaz._

import cats._
import cats.implicits._

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
