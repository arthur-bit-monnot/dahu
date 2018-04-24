package dahu.z3

import com.microsoft.z3._
import dahu.SFunctor
import dahu.model.ir._
import dahu.model.problem.IntBoolSatisfactionProblem
import dahu.model.problem.SatisfactionProblem.{ILazyTree, RootedLazyTree, TreeNode}
import dahu.model.types._
import dahu.solvers.PartialSolver
import dahu.utils.errors._

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

class TreeBuilder[X, F[_], G: ClassTag, Opt[_]](t: ILazyTree[X, F, Opt], f: F[G] => G)(
    implicit F: SFunctor[F],
    T: TreeNode[F]) {
  private val memo = mutable.HashMap[t.ID, G]()

  def build(k: t.ID): G = {
    val stack = mutable.Stack[t.ID]()
    def push(i: t.ID): Unit = {
      if(!memo.contains(i))
        stack.push(i)
    }
    push(k)
    while(stack.nonEmpty) {
      val a = stack.pop()
      val fa = t.getInt(a)
      if(T.children(fa).forall(memo.contains)) {
        val g = f(F.smap(fa)(memo))
        memo += ((a, g))
      } else {
        push(a)
        T.children(fa).foreach(push)
      }
    }
    memo(k)
  }
}

class Z3PartialSolver[X](_ast: RootedLazyTree[X, Total, cats.Id]) extends PartialSolver[X](_ast) {
  trait Tag

  val intBoolPb = new IntBoolSatisfactionProblem[X](ast)
  val root = intBoolPb.tree.root
  val tree = intBoolPb.tree.tree

  private val ctx = new Context()
  type OptTotal[T] = Option[Total[T]]

  val treeBuilder = new TreeBuilder(tree, Compiler.partialAlgebra(ctx))

  // Total
  def asExpr(id: X): Option[com.microsoft.z3.Expr] = {
    tree.getInternalID(id).map(internalID => treeBuilder.build(internalID))
  }
  def eval(id: X, model: com.microsoft.z3.Model): Option[Value] = {
    asExpr(id) match {
      case Some(e) =>
        model.eval(e, false) match {
          case i: IntNum =>
            ast.tree.getExt(id).typ match {
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
  }

  val compiled = Try(treeBuilder.build(root)) //hylo(intBoolPb.algebra.asFunction, algebra)(intBoolPb.root)
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

  override def nextSatisfyingAssignment(deadline: Long = -1): Option[X => Option[Value]] = {
    assert(model == null, "Z3 only support extraction of a single solution")
    val timeout =
      if(deadline < 0)
        None
      else if(deadline >= System.currentTimeMillis())
        Some((deadline - System.currentTimeMillis()).toInt)
      else
        Some(0)

    timeout match {
      case Some(0) => return None // deadline passed, to not attempt to solve
      case Some(t) =>
        val params = ctx.mkParams()
        params.add("timeout", t)
        solver.setParameters(params)
      case None =>
    }

    dahu.utils.debug.info("  Solving with Z3...")
    solver.check() match {
      case Status.SATISFIABLE =>
        model = solver.getModel
        Some(id => eval(id, model))
      case _ =>
        None
    }
  }

}

object Z3PartialSolver {

  object builder extends PartialSolver.Builder {
    override def apply[X](ast: RootedLazyTree[X, Total, cats.Id]): Z3PartialSolver[X] =
      new Z3PartialSolver[X](ast)
  }
}
