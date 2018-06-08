package dahu.z3

import cats._
import cats.implicits._
import com.microsoft.z3._
import dahu.graphs.TreeNode
import dahu.model.compiler.Algebras
import dahu.model.ir._
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.problem.{IDTop, IlazyForest, IntBoolSatisfactionProblem, LazyTree}
import dahu.model.types._
import dahu.solvers.PartialSolver
import dahu.utils.SFunctor
import dahu.utils.errors._

import scala.collection.mutable
import scala.concurrent.duration.Deadline
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

class TreeBuilder[X, F[_], G: ClassTag, Opt[_], I <: IDTop](
    t: IlazyForest[X, F, Opt, I],
    f: F[G] => G)(implicit F: SFunctor[F], T: TreeNode[F]) {
  private val memo = mutable.HashMap[t.ID, G]()

  def build(k: X)(implicit F: Functor[Opt]): Opt[G] = t.getTreeRoot(k).map(buildInternal)

  def buildInternal(k: t.ID): G = {
    val stack = mutable.Stack[t.ID]()
    def push(i: t.ID): Unit = {
      if(!memo.contains(i))
        stack.push(i)
    }
    push(k)
    while(stack.nonEmpty) {
      val a = stack.pop()
      val fa = t.internalCoalgebra(a)
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

class Z3PartialSolver[X](_ast: LazyTree[X, Total, IR, _]) extends PartialSolver[X] {
  implicit def treeNodeInstance: TreeNode[Total] = Total.treeNodeInstance
  private val ast = _ast.fixID

  private val intBoolPb = new IntBoolSatisfactionProblem[ast.ID](
    ast.tree.getTreeRoot(ast.root).valid,
    ast.tree.internalCoalgebra)
  private val tree = intBoolPb.tree.fixID

  private val ctx = new Context()
  private type OptTotal[T] = Option[Total[T]]

  private val treeBuilder = new TreeBuilder(tree.tree, Compiler.partialAlgebra(ctx))

  // Total
  def asExpr(id: X): Option[com.microsoft.z3.Expr] = {
    tree.tree
      .getTreeRoot(ast.tree.getTreeRoot(id).value)
      .map(internalID => treeBuilder.buildInternal(internalID))
  }
  def eval(id: X, model: com.microsoft.z3.Model): Option[Value] = {
    asExpr(id) match {
      case Some(e) =>
        model.eval(e, false) match {
          case i: IntNum =>
            ast.tree.getExt(id).value.typ match {
              case t: TagIsoInt[_] =>
                //Some(t.toValue(i.getInt))
                val v = i.getInt
                if(t.min <= v && v <= t.max) // TODO: make functions in tag iso int total
                  Some(t.toValue(v))
                else
                  None
              case _ => unexpected
            }
          case i: IntExpr =>
            None
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

  private val compiled = Try(treeBuilder.build(tree.root).get)
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
  private var model: Model = null

  override def nextSatisfyingAssignment(deadline: Option[Deadline]): Option[X => Option[Value]] = {
    assert(model == null, "Z3 only support extraction of a single solution")

    deadline match {
      case Some(dl) if dl.isOverdue => return None // deadline passed, do not attempt to solve
      case Some(t) =>
        val params = ctx.mkParams()
        params.add("timeout", t.timeLeft.toMillis.toInt)
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
    override def apply[X](ast: LazyTree[X, Total, IR, _]): Z3PartialSolver[X] =
      new Z3PartialSolver[X](ast)
  }
}
