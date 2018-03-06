package dahu.z3

import com.microsoft.z3._
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

import scala.util.{Failure, Success}
import scala.util.control.NonFatal

class Z3PartialSolver[AST <: TotalSubAST[_]](_ast: AST) extends PartialSolver[AST](_ast) {
  trait Tag

  val xxx: GrowableBiMap[FutureCellOpt] =
    GrowableBiMap.fromArray(ast.tree)(x => (Pending(x): FutureCellOpt))
  implicit val ev: <:<[ast.ID, xxx.K] = implicitly[<:<[Any, Any]].asInstanceOf[<:<[ast.ID, xxx.K]]

  type E = Total[xxx.K]
  type ConstGen = Total[xxx.K] => Total[xxx.K]

  sealed trait FutureCellOpt
  sealed trait CellOpt extends FutureCellOpt
  case class SupportedInput(value: InputF[xxx.K]) extends CellOpt {
    require(value.typ == Tag.ofBoolean)
  }
  case class CompatibleInput(value: InputF[xxx.K], t: TagIsoInt[_]) extends CellOpt {

    def constraint: Set[xxx.K] = Set(idOf(leq(cst(t.min), value)), idOf(leq(value, cst(t.max))))
  }
  case class SupportedConstant(value: CstF[xxx.K]) extends CellOpt {
    require(value.typ == Tag.ofBoolean)
    require(value.value.isInstanceOf[Boolean])

  }
  case class CompatibleConstant(value: CstF[xxx.K], t: TagIsoInt[_]) extends CellOpt {
    require(value.value.isInstanceOf[Int])
  }
  case class IntermediateExpression(value: ComputationF[xxx.K]) extends CellOpt {}
  case object Unsupported extends CellOpt
  case class Pending(e: Total[ast.ID]) extends FutureCellOpt

  private val True: E = CstF(Value(true), Tag.ofBoolean)

  private val supportedFunctions = Set[Fun[_]](int.Add,
                                               int.LEQ,
                                               int.EQ,
                                               int.Times,
                                               int.Negate,
                                               int.Min,
                                               bool.And,
                                               bool.Or,
                                               bool.Not)
  def supported(k: ast.ID): Boolean = toCell(k) match {
    case Unsupported => false
    case _           => true
  }

  val YYY: Total[ast.ID] => CellOpt = {
    case x @ CstF(v, Tag.ofBoolean) => SupportedConstant(x)
    case x @ CstF(v, t: TagIsoInt[_]) =>
      CompatibleConstant(CstF(Value(t.toIntUnsafe(v)), Tag.ofInt), t)
    case x @ InputF(name, Tag.ofBoolean)   => SupportedInput(x)
    case x @ InputF(name, t: TagIsoInt[_]) => CompatibleInput(InputF(name, Tag.ofInt), t)
    //in => and(leq(cst(t.min), in), leq(in, cst(t.max)))
    case x @ ComputationF(f, args, t: TagIsoInt[_])
        if supportedFunctions.contains(f) && args.forall(supported) =>
      IntermediateExpression(ComputationF(f, args.map(x => x: xxx.K), t))
    case x @ ComputationF(wf: WrappedFunction, args, t: TagIsoInt[_])
        if supportedFunctions.contains(wf.f) && args.forall(supported) =>
      YYY(ComputationF(wf.f, args, t)) // unwrap and retry

    case x =>
      x.typ match {
        case Tag.ofBoolean =>
          SupportedInput(InputF(x.toString, Tag.ofBoolean))
        case t: TagIsoInt[_] =>
          CompatibleInput(InputF(x.toString, Tag.ofInt), t)
        case _ => Unsupported
      }
  }

  def toCell(id: ast.ID): CellOpt = {
    xxx(id) match {
      case x: CellOpt => x
      case Pending(e) =>
        val tmp = YYY(e)
        xxx.update(id: xxx.K, tmp)
        tmp
    }
  }
  def idOf(e: CellOpt): xxx.K = xxx.keyOf(e)
  def idOf(e: E): xxx.K = idOf(YYY(e.asInstanceOf[Total[ast.ID]]))

  def and(conjuncts: E*): E = {
    assert(conjuncts.forall(c => c.typ == Tag.ofBoolean))
    ComputationF(bool.And, conjuncts.toSeq.map(x => idOf(x)), Tag.ofBoolean)
  }
  def leq(lhs: E, rhs: E): E = {
    ComputationF(int.LEQ, Seq(lhs, rhs).map(x => idOf(x)), Tag.ofBoolean)
  }
  def cst(n: Int): E = CstF(Value(n), Tag.ofInt)

  for(i <- ast.tree.domain) {
    idOf(toCell(i))
  }

  for(x <- xxx.content.keys.asInstanceOf[Array[Int]])
    print(" " + x)
  println
  for(i <- xxx.domain) {

    println(f"$i%2s: ${ast.tree.get(i).map(_.toString).getOrElse("---")}    --  ${xxx(i)}")
  }
  println("STOOOOOOOOP")

  def gatherConstraints(k: xxx.K): Set[xxx.K] = xxx(k) match {
    case i: CompatibleInput        => i.constraint
    case IntermediateExpression(e) => e.args.toSet.flatMap(gatherConstraints)

    case Unsupported =>
      unexpected("Gathering constraints from unsupported, we ned to by pass it...")
    case Pending(_) => unexpected("All pending things should have been processed already")
    case _          => Set()
  }
  val rootValue = ComputationF(bool.And,
                               (gatherConstraints(ast.root) ++ Set(ast.root: xxx.K)).toSeq,
                               bool.And.tpe)
  val root = idOf(rootValue)

  val coalg: xxx.K => Option[Total[xxx.K]] = k => {
    val res = xxx(k) match {
      case IntermediateExpression(e) => Some(e)
      case CompatibleInput(v, _)     => Some(v)
      case SupportedInput(v)         => Some(v)
      case CompatibleConstant(v, _)  => Some(v)
      case SupportedConstant(v)      => Some(v)
      case Unsupported               => None
      case Pending(_)                => unexpected("All pending things should have been processed already")
    }
    res
  }

  val tmp =
    ArrayMap
      .buildWithKey(xxx.domain)(k => coalg(k))
      .collect { case Some(v) => v }

  val zzz = tmp.asInstanceOf[ArrayMap.Aux[tmp.K, Total[tmp.K]]]

  assert(zzz.isInDomain(root))
  val realRoot = root.asInstanceOf[zzz.K]

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
