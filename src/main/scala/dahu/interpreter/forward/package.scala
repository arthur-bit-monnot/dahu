package dahu.interpreter

import dahu.interpreter.ast.AST
import cats._
import cats.implicits._
import dahu.dataframe.ColumnView
import dahu.utils.Errors._

package object forward {

  def evaluate(ast: AST, environment: Environment): Res[V] = {
    val executionState = new ExecutionState(ast)

    val binds: Res[List[(VarID, V)]] = environment.inputs.toList.map {
      case (variable, value) => ast.inputOf(variable).flatMap(ast.address).map((_, value))
    }.sequence
    val evaluated = binds match {
      case Right(bs) =>
        bs.foreach { case (id, value) => executionState.update(id, value) }
        executionState.get(ast.head).toRight(new Throwable("Unset variable"))
      case Left(x) => return Left(x)
    }


    val prg = ExecutingPrg(ast)

    val updated = environment.inputs.toSeq.foldLeft(Right(prg): Either[Throwable, ExecutingPrg]) {
      case (pOpt, (variable, value)) =>
        for {
          p     <- pOpt
          input <- p.ast.inputOf(variable)
          vid   <- p.ast.address(input)
          x     <- p.update(vid, value)
        } yield x
    }
    val result = updated.flatMap(_.get(ast.head))
    assert(result == evaluated)
    result
  }

  def evaluated(ast: AST, environment: Environment): ColumnView[V] = {
    val executionState = new ExecutionState(ast)

    val binds: Res[List[(VarID, V)]] = environment.inputs.toList.map {
      case (variable, value) => ast.inputOf(variable).flatMap(ast.address).map((_, value))
    }.sequence
    val evaluated = binds match {
      case Right(bs) =>
        bs.foreach { case (id, value) => executionState.update(id, value) }
        executionState.get(ast.head).toRight(err("Unset variable"))
      case Left(x) => Left(x)
    }
    val mem = evaluated
      .flatMap(_ => executionState.memory.asVector.toRight(err("Program not entirely evaluated.")))

    executionState
    ???
  }

}
