package dahu.interpreter

import dahu.interpreter._
import dahu.interpreter.ast.AST

package object forward {

//  def evaluate(ast: AST, environment: Environment): Res[V] = {
//    val prg = ExecutingPrg(ast, PrgState.init(ast.code))
//    val updated = environment.inputs.toSeq.foldLeft(Right(prg): Either[Throwable, ExecutingPrg]) {
//      case (pOpt, (variable, value)) =>
//        for {
//          p     <- pOpt
//          input <- p.ast.inputOf(variable)
//          vid   <- p.ast.address(input)
//          x     <- p.update(vid, value)
//        } yield x
//    }
//    updated.flatMap(_.get(ast.head))
//  }

}
