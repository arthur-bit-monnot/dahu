package dahu.z3

import cats._
import cats.implicits._
import com.microsoft.z3._
import dahu.graphs.TreeNode
import dahu.model.compiler.Algebras
import dahu.model.ir._
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.problem.{Context => _, _}
import dahu.model.types._
import dahu.solvers.PartialSolver
import dahu.utils.SFunctor
import dahu.utils.errors._
import dahu.utils._

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

class Z3PartialSolver[X, AstID <: IDTop](ast: LazyTree[X, Total, IR, AstID])
    extends PartialSolver[X, AstID] {
  implicit def treeNodeInstance: TreeNode[Total] = Total.treeNodeInstance
  private val ast = _ast.fixID
//  Inference2Sat.processTargettingTrue(ast.mapExternal[cats.Id](_.valid))

//  private val t2 = ast.tree.transform(SatisfactionProblem.Optimizations.implicationGrouper)
//  private val printable = t2.cata(Algebras.printAlgebraTree)
//  println()
//  println(printable.get(ast.root).valid.mkString(120))
//  println("e")
//  System.exit(0)

  private val intBoolPb = new IntBoolSatisfactionProblem[ast.ID](
    ast.tree.getTreeRoot(ast.root).valid,
    ast.tree.internalCoalgebra)
  private val tree = intBoolPb.tree.fixID

  private val ctx = new Context()
  private type OptTotal[T] = Option[Total[T]]

  private val treeBuilder = new TreeBuilder(tree.tree, Compiler.partialAlgebra(ctx))

  // Total
  private def asExpr(id: X): Option[com.microsoft.z3.Expr] = {
    asExprInternal(ast.tree.getTreeRoot(id).value)
  }
  private def asExprInternal(id: AstID): Option[com.microsoft.z3.Expr] = {
    tree.tree
      .getTreeRoot(id)
      .map(internalID => treeBuilder.buildInternal(internalID))
  }
  def eval(id: X, model: com.microsoft.z3.Model): Option[Value] =
    evalInternal(ast.tree.getTreeRoot(id).value, model)
  def evalInternal(id: AstID, model: com.microsoft.z3.Model): Option[Value] = {
    asExprInternal(id) match {
      case Some(e) =>
        model.eval(e, false) match {
          case i: IntNum =>
            ast.tree.internalCoalgebra(id).typ match {
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
    nextSatisfyingAssignmentInternal(deadline) match {
      case Some(f) => Some((x: X) => f(ast.tree.getTreeRoot(x).value))
      case None    => None
    }
  }

  def nextSatisfyingAssignmentInternal(
      deadline: Option[Deadline]): Option[AstID => Option[Value]] = {
    assert(model == null, "Z3 only support extraction of a single solution")

    deadline match {
      case Some(dl) if dl.isOverdue => return None // deadline passed, do not attempt to solve
      case Some(t) =>
        val params = ctx.mkParams()
        params.add("timeout", t.timeLeft.toMillis.toInt)
        solver.setParameters(params)
      case None =>
    }

    debug.info("  Solving with Z3...")
    solver.check() match {
      case Status.SATISFIABLE =>
        debug.info("  Satisfiable model found.")
        model = solver.getModel
        Some(id => evalInternal(id, model))
      case _ =>
        debug.info("  no model found.")
        None
    }
  }

}

object Z3PartialSolver {

  object builder extends PartialSolver.Builder {
    override def apply[X, AstID <: IDTop](
        ast: LazyTree[X, Total, IR, AstID]): Z3PartialSolver[X, AstID] =
      new Z3PartialSolver[X, AstID](ast)
  }
}
