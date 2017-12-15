package dahu

import dahu.interpreter.ast.AST
import dahu.recursion.{ComputationF, CstF, InputF, ResultF}

import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal
import cats._
import cats.syntax._
import cats.implicits._

package object interpreter {

  type VarID = Int
  type FunID = Int
  type Expr  = ResultF[VarID]
  type Fun   = ComputationF[VarID]
  type Input = InputF[VarID]
  type Cst   = CstF[VarID]

  final case class Code(forward: Vector[Expr], backward: Map[Expr, VarID]) {
    require(backward.toSeq.forall { case (expr, id) => forward(id) == expr })
  }

  type VarName = String

  type Res[T] = Either[Throwable, T]

  sealed abstract class Language
  final case class SetVar(input: Input, value: V) extends Language
  final case class GetVar(output: VarID)          extends Language

  type Hist = List[SetVar]

  case class Environment(inputs: Map[VarName, V])
  object Environment {
    def apply(binds: (VarName, V)*): Environment = Environment(binds.toMap)
  }

  final case class Err(msg: String) extends Throwable(msg)

//  def evaluate(ast: AST, environment: Environment): Res[V] = {
//    def get(address: VarID): Res[V] =
//      ast.at(address).flatMap {
//        _ match {
//          case x: Input =>
//            environment.inputs.get(x.name) match {
//              case Some(x) => Right(x)
//              case None    => Left(Err(s"Input $x was not set"))
//            }
//          case x: Cst => Right(x.value)
//          case x: Fun => x.args.traverse(get(_)).map(x.fun.compute(_))
//        }
//      }
//
//    get(ast.head)
//  }

  type V = Any

}
