package dahu.z3

import cats._
import cats.implicits._
import com.microsoft.z3._
import dahu.graphs.TreeNode
import dahu.model.compiler.Algebras
import dahu.model.ir._
import dahu.model.problem.SatisfactionProblem.IR
import dahu.model.problem._
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

class TreeBuilder[X, F[_], G: ClassTag, Opt[_], I <: IDTop](t: OpenASG[X, F, Opt, I], f: F[G] => G)(
    implicit F: SFunctor[F],
    T: TreeNode[F],
    ct: ClassTag[F[I]]
) {
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

//        println(fa)
//        val fb = F.smap(fa)(t.internalCoalgebra)
//        println(fb)
//        println()
//        if(fa.toString.startsWith("add(4")) {
//          println("BREAK")
//        }
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

class Z3PartialSolver[X, AstID <: IDTop](ast: LazyTree[X, Total, cats.Id, AstID])
    extends PartialSolver[X, AstID] {
  implicit def treeNodeInstance: TreeNode[Total] = Total.treeNodeInstance

  val isSupported: AstID => Boolean = IntBoolUtils.isExprOfIntBool(ast.tree)

  private val ctx = new Context()
  private type OptTotal[T] = Option[Total[T]]

  private val treeBuilder = new TreeBuilder(ast.tree, Compiler.partialAlgebra(ctx))

  def addClause(e: Any): Unit = ???

  // Total
  private def asExpr(id: X): Option[com.microsoft.z3.Expr] = {
    asExprInternal(ast.tree.getTreeRoot(id))
  }
  private def asExprInternal(id: AstID): Option[com.microsoft.z3.Expr] = {
    if(isSupported(id)) {
      Some(treeBuilder.buildInternal(id))
    } else {
      None
    }
  }
  def eval(id: X, model: com.microsoft.z3.Model): Option[Value] =
    evalInternal(ast.tree.getTreeRoot(id), model)
  def evalInternal(id: AstID, model: com.microsoft.z3.Model): Option[Value] = {
    asExprInternal(id) match {
      case Some(e) =>
        model.eval(e, false) match {
          case i: IntNum =>
            ast.tree.internalCoalgebra(id).typ match {
              case t: RawInt =>
                assert(!t.isBoolean)
                //Some(t.toValue(i.getInt))
                val v = i.getInt
                assert(
                  t.min <= v && v <= t.max,
                  "Value not in specified bounds, Note that this is in fact acceptable if the value is not used")
                if(t.min <= v && v <= t.max) // TODO: make functions in tag iso int total
                  Some(Value(v))
                else {
                  None
                }
              case _ => unexpected
            }
          case i: IntExpr =>
            // not constrained, if decision variable just return the first value in the domain
            ast.tree.internalCoalgebra(id) match {
              case InputF(_, typ: RawInt) =>
                assert(typ.isBoolean)
                Some(Value(typ.min))

              case _ => None
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

  private val compiled = Try(treeBuilder.build(ast.root))
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

  // add the constraints corresponding to the domain of variables
  // for a variable X in scope S, the domain essentially encode:
  //     present(S) ==> value(X) in domain(X)
  ast.nodes.foreach {
    case (id, InputF(name, typ: RawInt)) if typ.isInt =>
      assert(!typ.isBoolean)
      if(typ.min <= typ.max) {
        // domain is non empty, impose inconditionally
        solver.add(
          ctx.mkLe(ctx.mkInt(typ.min), treeBuilder.buildInternal(id).asInstanceOf[ArithExpr]))
        solver.add(
          ctx.mkLe(treeBuilder.buildInternal(id).asInstanceOf[ArithExpr], ctx.mkInt(typ.max)))
      } else {
        // domain is empty, make sure the variable is never use by making its scope absent
        solver.add(
          ctx.mkNot(
            treeBuilder.build(name.id.scope.present.asInstanceOf[X]).asInstanceOf[BoolExpr]))
      }
    case _ =>
  }

  private var model: Model = null

  override def nextSatisfyingAssignment(deadline: Option[Deadline]): Option[X => Option[Value]] = {
    nextSatisfyingAssignmentInternal(deadline) match {
      case Some(f) => Some((x: X) => f(ast.tree.getTreeRoot(x)))
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
        ast: LazyTree[X, Total, cats.Id, AstID]): Z3PartialSolver[X, AstID] =
      new Z3PartialSolver[X, AstID](ast)
  }
}
