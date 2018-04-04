package dahu.z3

import com.microsoft.z3._
import dahu.maps.SubSubInt
import dahu.model.ir._
import dahu.model.problem.IntBoolSatisfactionProblem
import dahu.model.types._
import dahu.solvers.PartialSolver
import dahu.utils.errors._
import dahu.recursion.Recursion.hylo

import scala.util.{Failure, Success}
import scala.util.control.NonFatal

class Z3PartialSolver[AST <: TotalSubAST[_]](_ast: AST) extends PartialSolver[AST](_ast) {
  trait Tag

  val intBoolPb = new IntBoolSatisfactionProblem[ast.type](ast)

  // Total
  def asExpr(id: ast.ID): Option[com.microsoft.z3.Expr] = {
    intBoolPb.algebra.get(id) match {
      case Some(e) =>
        hylo(intBoolPb.algebra.asFunction, algebra)(id.asInstanceOf[intBoolPb.K]).toOption
      case None => None
    }
  }
  def eval(id: ast.ID, model: com.microsoft.z3.Model): Option[Value] =
    asExpr(id) match {
      case Some(e) =>
        model.eval(e, false) match {
          case i: IntNum =>
            ast.tree(id).typ match {
              case t: TagIsoInt[_] => Some(t.toValue(i.getInt))
              case _               => unexpected
            }
          case b: BoolExpr =>
            b.getBoolValue match {
              case enumerations.Z3_lbool.Z3_L_FALSE => Some(Value(false))
              case enumerations.Z3_lbool.Z3_L_TRUE  => Some(Value(true))
              case enumerations.Z3_lbool.Z3_L_UNDEF => None
              case _                                => unexpected
            }

        }

      case None => None
    }

  override type K = SubSubInt[ast.ID, Tag]

  private val ctx = new Context()
  private val algebra = Compiler.algebra(ctx)
  val compiled = hylo(intBoolPb.algebra.asFunction, algebra)(intBoolPb.root)
  private val satProblem = compiled match {
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
    println("Check with Z3...")
    solver.check() match {
      case Status.SATISFIABLE =>
        model = solver.getModel
        val partial = (id: ast.VID) => {
          eval(id, model)
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
