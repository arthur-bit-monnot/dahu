package dahu.z3

import cats.{Eval, Later}
import com.microsoft.z3._
import dahu.graphs.DAG
import dahu.maps.{ArrayMap, SubSubInt}
import dahu.maps.growable.GrowableBiMap
import dahu.model.functions.Fun
import dahu.model.input.dsl.WrappedFunction
import dahu.model.ir._
import dahu.model.math.{bool, int}
import dahu.model.types._
import dahu.solvers.PartialSolver
import dahu.utils.errors._
import dahu.recursion.Recursion.hylo

import scala.collection.mutable
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

class Tmp[AST <: TotalSubAST[_]](val ast: AST) {

  type K = builder.K

  sealed trait CellOpt
  case class SupportedInput(value: InputF[K]) extends CellOpt {
    require(value.typ == Tag.ofBoolean)
  }
  case class CompatibleInput(value: InputF[K], t: TagIsoInt[_]) extends CellOpt {
    def constraint: Set[K] = Set(idOf(leq(cst(t.min), this)), idOf(leq(this, cst(t.max))))
  }
  case class SupportedConstant(value: CstF[K]) extends CellOpt {
    require(value.typ == Tag.ofBoolean)
    require(value.value.isInstanceOf[Boolean])

  }
  case class CompatibleConstant(value: CstF[K], t: TagIsoInt[_]) extends CellOpt {
    require(value.value.isInstanceOf[Int])
  }
  case class IntermediateExpression(value: ComputationF[K]) extends CellOpt {}
  case object Unsupported extends CellOpt

  private val supportedFunctions = Set[Fun[_]](int.Add,
                                               int.LEQ,
                                               int.EQ,
                                               int.Times,
                                               int.Negate,
                                               int.Min,
                                               bool.And,
                                               bool.Or,
                                               bool.Not)

  implicit val dag: DAG[Total, ast.ID] = new DAG[Total, ast.ID] {
    override def algebra: ast.ID => Total[ast.ID] = ast.tree.asFunction

    override def children(graph: Total[ast.ID]): Set[ast.ID] = graph match {
      case ProductF(members, _)     => members.toSet
      case ComputationF(_, args, _) => args.toSet
      case _                        => Set()
    }
  }

  // translate all nodes of the AST to cells.
  // we need to take since we want to maintain the same IDs and a node typically refers to its subnodes.
  val leavesToRoot = dag.topologicalOrder(ast.tree.domain.toScalaSet()).reverse
  val originalCells = mutable.Map[ast.ID, CellOpt]()
  for(i <- leavesToRoot) {
    originalCells += ((i, TRANS(ast.tree(i))))
  }

  def sup(e: ast.ID) = originalCells(e) match {
    case Unsupported => false
    case _           => true
  }
  def TRANS: Total[ast.ID] => CellOpt = {
    case x @ CstF(v, Tag.ofBoolean) => SupportedConstant(x)
    case x @ CstF(v, t: TagIsoInt[_]) =>
      CompatibleConstant(CstF(Value(t.toIntUnsafe(v)), Tag.ofInt), t)
    case x @ InputF(name, Tag.ofBoolean)   => SupportedInput(x)
    case x @ InputF(name, t: TagIsoInt[_]) => CompatibleInput(InputF(name, Tag.ofInt), t)
    case x @ ComputationF(f, args, t: TagIsoInt[_])
        if supportedFunctions.contains(f) && args.forall(sup) =>
      IntermediateExpression(ComputationF(f, args.map(x => x: K), t))
    case x @ ComputationF(wf: WrappedFunction, args, t: TagIsoInt[_])
        if supportedFunctions.contains(wf.f) && args.forall(sup) =>
      TRANS(ComputationF(wf.f, args, t)) // unwrap and retry

    case x =>
      x.typ match {
        case Tag.ofBoolean =>
          SupportedInput(InputF(x.toString, Tag.ofBoolean))
        case t: TagIsoInt[_] =>
          CompatibleInput(InputF(x.toString, Tag.ofInt), t)
        case _ => Unsupported
      }
  }

  // initialize with direct mapping from the ast to Cells
  val builder: GrowableBiMap[CellOpt] =
    GrowableBiMap.fromArray(ast.tree)(x => originalCells(x))

  // todo: we should provide this automatically
  // by construction, each ast.ID is also a member of K (ast.ID is a subset of K)
  implicit def ev: <:<[ast.ID, K] = implicitly[Any <:< Any].asInstanceOf[ast.ID <:< K]

  def toCell(id: ast.ID): CellOpt = builder(id)

  /** Retrieves or generates an ID for this cell. If e is absent from builder, it is added to it with the generated id. */
  def idOf(e: CellOpt): K = builder.keyOf(e)

  def and(conjuncts: CellOpt*): CellOpt = {
    IntermediateExpression(
      ComputationF(bool.And, conjuncts.toSeq.map(x => idOf(x)), Tag.ofBoolean)
    )
  }
  def leq(lhs: CellOpt, rhs: CellOpt): CellOpt = {
    IntermediateExpression(
      ComputationF(int.LEQ, Seq(lhs, rhs).map(x => idOf(x)), Tag.ofBoolean)
    )
  }
  def cst(n: Int): CellOpt = CompatibleConstant(CstF(Value(n), Tag.ofInt), Tag.ofInt)

  def gatherConstraints(k: K): Set[K] = builder(k) match {
    case i: CompatibleInput        => i.constraint
    case IntermediateExpression(e) => e.args.toSet.flatMap(gatherConstraints)

    case Unsupported =>
      unexpected("Gathering constraints from unsupported, we ned to by pass it...")
    case _ => Set()
  }
  // the root value, is the conjunction of the original root and all constraints placed on inputs.
  val rootValue = IntermediateExpression(
    ComputationF(bool.And, (gatherConstraints(ast.root) + (ast.root: K)).toSeq, bool.And.tpe))
  val root = idOf(rootValue)

  val coalg: K => Option[Total[K]] = k => {
    val res = builder(k) match {
      case IntermediateExpression(e) => Some(e)
      case CompatibleInput(v, _)     => Some(v)
      case SupportedInput(v)         => Some(v)
      case CompatibleConstant(v, _)  => Some(v)
      case SupportedConstant(v)      => Some(v)
      case Unsupported               => None
    }
    res
  }

  // from builder, gather all expression that can be represented (i.e. int or bool)
  val tmp =
    ArrayMap
      .buildWithKey(builder.domain)(k => coalg(k))
      .collect { case Some(v) => v }

  // an algebra X => Total[X], where Total[X] is guaranteed to be representable.
  val result = tmp.asInstanceOf[ArrayMap.Aux[tmp.K, Total[tmp.K]]]

  assert(result.isInDomain(root))
  val realRoot = root.asInstanceOf[result.K]

  def get = (result, realRoot)

}

class Z3PartialSolver[AST <: TotalSubAST[_]](_ast: AST) extends PartialSolver[AST](_ast) {
  trait Tag

  val tmp = new Tmp[ast.type](ast)
  val (zzz, realRoot) = tmp.get

  // Total
  def asExpr(id: ast.ID): Option[com.microsoft.z3.Expr] = {
    zzz.get(id) match {
      case Some(e) => hylo(zzz.asFunction, algebra)(id.asInstanceOf[zzz.K]).toOption
      case None    => None
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

        }

      case None => None
    }

  override type K = SubSubInt[ast.ID, Tag]

  private val ctx = new Context()
  private val algebra = Compiler.algebra(ctx)
  val compiled = hylo(zzz.asFunction, algebra)(realRoot) //  hylo(ast.tree.asFunction, algebra)(ast.root)
  println(compiled)
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
