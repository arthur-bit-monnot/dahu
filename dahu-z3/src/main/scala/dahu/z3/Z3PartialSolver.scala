package dahu.z3

import com.microsoft.z3._
import dahu.maps.SubSubInt
import dahu.model.ir._
import dahu.model.types._
import dahu.solvers.PartialSolver
import dahu.utils.errors._
import dahu.recursion.Recursion.hylo

import scala.util.{Failure, Success}
import scala.util.control.NonFatal

class Z3PartialSolver[AST <: TotalSubAST[_]](_ast: AST) extends PartialSolver[AST](_ast) {
  trait Tag

  override type K = SubSubInt[ast.ID, Tag]

  private val ctx = new Context()
  private val algebra = Compiler.algebra(ctx)
  private val satProblem = hylo(ast.tree.asFunction, algebra)(ast.root) match {
    case Success(x: BoolExpr) => x
    case Failure(NonFatal(e)) => unexpected("Failure while parsing Z3. Cause: ", e)
    case x                    => unexpected(s"Was expecting a boolean expression but got: $x")
  }

  def valueOf(expr: Expr, model: Model): Value = {
    model.eval(expr, false) match {
      case x: IntNum => Value(x.getInt)
    }
  }

  private val solver = ctx.mkSolver()
  solver.add(satProblem)
  var model: Model = null

  override def nextSatisfyingAssignment(): Option[ast.PartialAssignment] = {
    assert(model == null, "Z3 only support extraction of a single solution")
    solver.check() match {
      case Status.SATISFIABLE =>
        model = solver.getModel
        val partial = (id: ast.VID) => {
          hylo(ast.tree.asFunction, algebra)(id) match {
            case Success(expr) => Some(valueOf(expr, model))
            case _             => None
          }
        }
        Some(partial)
      case _ =>
        None
    }
  }
}

object Z3PartialSolver {

  object builder extends PartialSolver.Builder {
    override def apply(ast: TotalSubAST[_]): Z3PartialSolver[ast.type] =
      new Z3PartialSolver[ast.type](ast)
  }
}
